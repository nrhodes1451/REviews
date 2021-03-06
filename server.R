shinyServer(function(input, output, clientData, session) {
  options(warn =-1)

  observe({
    # Error message ----
    display_error <- function(message){
      renderUI({
        HTML(paste0(
          "<div class=\"error-notification-overlay\"></div>
        <div class=\"shiny-notification shiny-notification-error\">
          <div class=\"shiny-notification-close\"
            onclick=\"$('.error-notification-overlay').hide();
                    $('.shiny-notification-error').hide();\">
            <i class=\"fa fa-times-circle\"></i>
          </div>
          <div class=\"shiny-notification-content\">
            <div class=\"shiny-notification-content-text\">",
          message,
          "</div>
            <div class=\"shiny-notification-content-action\"></div>
          </div>
        </div>"
        ))
      })
    }

    # Sidebar ----
    output$txt_dataset <- renderText(paste("Dataset:", global$dataset))

    req(input$file1)

    tryCatch(
      {
        if(str_sub(tolower(input$file1$datapath), -4,-1) != ".csv"){
          output$error_message <- display_error("
            <strong>Invalid filetype:</strong><br/>
              Reverting to demo dataset.")
        }
        else{
          df <- read_csv(input$file1$datapath)

          names(df)[1] <- tolower(names(df)[1])

          date_error <- FALSE
          if(names(df)[1]!="date"){
            output$error_message <- display_error("
            <strong>Error: date column not found</strong><br/>
              Please provide a CSV file with dates in the first column.<br/>
              Reverting to demo dataset.")
          }
          else if(class(df$date)=="character"){
            df$date <- dmy(df$date)
            if(sum(is.na(df$date))>0) date_error <- TRUE
          }
          if(date_error){
            output$error_message <- display_error("
            <strong>Invalid date format</strong><br/>
              Dates should be in either of the following formats.<br>
              <ul><li>'2019-01-31'</li><li>'31/01/2019'</li></ul><br/>
              Reverting to demo dataset.")
          }
          else{
            global$model_data <- df
            global$start_date <- min(df$date)
            global$end_date <- max(df$date)

            global$model <- ev_model$new(
              "model",
              global$start_date,
              global$end_date,
              now()
            )
          }
        }
      },
      error = function(e) {
        display_error(e)
      }
    )

    # Explorer ----
    updateSelectInput(session, "inp_exp_src",
                      choices = names(global$model_data[-1]),
                      selected = names(global$model_data[2]))
    # Explorer plot
    output$render_explorer <- renderPlotly({
      if(input$inp_exp_src == ''){
        plotly_empty()
      }
      else{
        src_var <- input$inp_exp_src
        chart_data <- global$model_data %>%
          select(date, src_var) %>%
          filter(date >= global$start_date)
        plot_ly(
          x = chart_data[[1]],
          y = chart_data[[2]],
          name=src_var,
          type="scatter",
          mode="lines",
          line = list(color = global_options$charts$colors$purple)) %>%
          layout(
            margin=list(r=60),
            title = src_var,
            xaxis = list(
              title = "",
              hoverformat="%e %b %y"
            ),
            yaxis = list(
              showgrid = FALSE,
              showline=TRUE,
              range=c(min(0,chart_data[[2]]),
                      max(chart_data[[2]]))
            ),
            font=global_options$charts$font,
            legend = list(orientation = 'h')
          )
      }
    })
    # 12month plot
    output$render_explorer_latest_year <- renderPlotly({
      if(input$inp_exp_src == ''){
        plotly_empty()
      }
      else{
        src_var <- input$inp_exp_src
        chart_data <- global$model_data %>%
          select(date, src_var) %>%
          filter(date >= global$start_date) %>%
          filter(date >= (max(global$model_data$date)-years(1)))
        plot_ly(
          x = chart_data[[1]],
          y = chart_data[[2]],
          name=src_var,
          type="scatter",
          mode="lines",
          line = list(color = global_options$charts$colors$purple)) %>%
          layout(
            margin=list(r=60),
            title = src_var,
            xaxis = list(
              title = "",
              hoverformat="%e %b %y"
            ),
            yaxis = list(
              showgrid = FALSE,
              showline=TRUE,
              range=c(min(0,chart_data[[2]]),
                      max(chart_data[[2]]))
            ),
            font=global_options$charts$font,
            legend = list(orientation = 'h')
          )
      }
    })
    # 12month plot
    output$render_explorer_hy <- renderPlotly({
      if(input$inp_exp_src == ''){
        plotly_empty()
      }
      else{
        vname <- input$inp_exp_src
        chart_data <- global$model_data %>%
          select(date, vname) %>%
          filter(date >= global$start_date) %>%
          filter(date >= (max(global$model_data$date)-years(1))) %>%
          mutate(date = paste0(year(date),
                               "-H",
                               as.integer(month(date)/7+1))) %>%
          group_by(date) %>%
          summarise_all(funs(sum)) %>%
          ungroup
        plot_ly(
          x = chart_data[[1]],
          y = chart_data[[2]],
          name=vname,
          type="bar",
          marker = list(color = global_options$charts$colors$purple)) %>%
          config(displayModeBar = F) %>%
          layout(
            margin=list(r=60),
            xaxis = list(
              title = "",
              tickangle = -45,
              tickfont = list(size=10)
            ),
            yaxis = list(
              showgrid = FALSE,
              showline= FALSE
            ),
            font=global_options$charts$font
          )
      }
    })

    # Download
    output$dl_explorer_cht <- downloadHandler(
      filename = paste0("data.csv"),
      content = function(file) {
        write_csv(global$model_data[c("date", input$inp_exp_src)],file)
      }
    )

    # Modelling

    # Decomp ----
    observe(updateSelectInput(session, "inp_decomp_model",
                      choices = str_remove_all(dir("models"), ".RDS")))
    # Decomp Event Handlers ----

    estimate_model <- function(script){
      # Parse model script
      if(global$model$parse_model(script, global$model_data)){
        global$model$run_regression()
        # Render Coeffs
        output$coeffs_table <- render_coeffs(global$model)
        # Render Diagnostics
        output$diagnostics_table <- render_diagnostics(global$model)
        # Render Actual vs. Fitted
        output$render_decomp_avm <- render_decomp_avm(global$model)
        # Render Decomp
        output$render_decomp_dc <- render_decomp_dc(global$model)
      }
      else{
        print(global$model$message)
        message <- global$model$message
        if(!is.null(message)) output$error_message <- display_error(message)
      }
    }
    # Load Model
    observeEvent(input$btn_decomp_model_load, {
      global$model <- readRDS(paste0("models/",
                                     input$inp_decomp_model,
                                     ".RDS"))
      script <- global$model$model_script %>%
        paste(collapse="\n")
      updateTextInput(session, "txt_decomp_eqn", value=script)
      estimate_model(script)
    })
    # Save Model
    observeEvent(input$btn_decomp_model_save, {
      # Parse model script
      if(global$model$parse_model(input$txt_decomp_eqn, global$model_data)){
        global$model %>% saveRDS(paste0("models/", global$model$kpi, ".RDS"))
      }
      else{
        message <- global$model$message
        if(!is.null(message)) output$error_message <- display_error(message)
      }
    })
    # Estimate Model
    observeEvent(input$btn_decomp_model_est, estimate_model(input$txt_decomp_eqn))
    # Coeff table ----
    render_coeffs <- function(model){
      df <- model$get_model()
      return(renderRHandsontable({
        rhandsontable(df,
          readOnly = TRUE,
          stretchH = "all",
          renderAllRows = TRUE,
          rowHeaders = NULL)
      }))

      # # Highlight values that have changed in the table
      # changes <- anti_join(cbind(1:nrow(decomp$mapping), decomp$mapping),
      #                      compare,
      #                      by=names(decomp$mapping)[-grep("Parameter", names(decomp$mapping))])
      #
      # # Create JavasScript array from R dataframe index
      # changes <- paste(changes[[1]]-1, collapse=",")
      # changes <- paste0("[",changes,"]")
      #
      # # Colour for changed row highlight
      # change_colour <- "lightyellow"
      #
      # # Renderers tp highlight rows if values have changed
      # render_text_changes = paste0(
      #   "function(instance, td, row, col, prop, value, cellProperties) {
      #     Handsontable.TextCell.renderer.apply(this, arguments);
      #     if($.inArray(row, ",changes,")>=0) td.style.background='",change_colour,"';
      #   }")
      # render_numeric_changes = paste0(
      #   "function(instance, td, row, col, prop, value, cellProperties) {
      #     Handsontable.NumericCell.renderer.apply(this, arguments);
      #     if($.inArray(row, ",changes,")>=0) td.style.background='",change_colour,"';
      #   }")
      # render_checkbox_changes = paste0(
      #   "function(instance, td, row, col, prop, value, cellProperties) {
      #     Handsontable.CheckboxCell.renderer.apply(this, arguments);
      #     if($.inArray(row, ",changes,")>=0) td.style.background='",change_colour,"';
      #   }")
      # render_dropdown_changes = paste0(
      #   "function(instance, td, row, col, prop, value, cellProperties) {
      #     Handsontable.DropdownCell.renderer.apply(this, arguments);
      #     if($.inArray(row, ",changes,")>=0) td.style.background='",change_colour,"';
      #   }")

      return(renderRHandsontable({
        rhandsontable(df,
                      readOnly = TRUE,
                      stretchH = "all",
                      renderAllRows = TRUE) %>%
          hot_cols(colWidths = 100,
                   columnSorting = TRUE,
                   fixedColumnsLeft = 2) # %>%
          # Render changed values using JavaScript
          # hot_col(1:length(decomp$mapping), renderer = render_text_changes) %>%
          # hot_col("ReferencePoint", readOnly = !decomp$active,
          #         type="dropdown", source=c("0","Min","Max"), strict=T,
          #         renderer = render_dropdown_changes) %>%
          # hot_col("Adstock", readOnly = !decomp$active, format="0",
          #         renderer = render_numeric_changes) %>%
          # hot_validate_numeric("Adstock",0,99) %>%
          # hot_col("Denominator", readOnly = !decomp$active, format="0",
          #         renderer = render_numeric_changes) %>%
          # hot_validate_numeric("Denominator",0) %>%
          # hot_col("GroupName", readOnly = !decomp$active,
          #         type="dropdown", source=groups, strict=T,
          #         renderer = render_dropdown_changes) %>%
          # hot_col("SuperGroupName", readOnly = !decomp$active,
          #         type="dropdown", source=supergroups, strict=T,
          #         renderer = render_dropdown_changes) %>%
          # hot_col("IsNestedBaseGroup", type="checkbox", readOnly = !decomp$active,
          #         renderer = render_checkbox_changes)
      }))
    }
    # Diagnostics table ----
    render_diagnostics <- function(model){
      if(is.null(model$get_diagnostics())){
        return(renderRHandsontable({
          rhandsontable(data.frame("No model loaded"))}))
      }
      df <- model$get_diagnostics() %>%
        data.frame %>%
        t %>%
        as_tibble(rownames="metric")
      names(df)[2] <- "value"
      return(renderRHandsontable({
        rhandsontable(df,
                      readOnly = TRUE,
                      stretchH = "all",
                      renderAllRows = TRUE,
                      rowHeaders = NULL)
      }))
    }
    # Decomp Plots ----
    render_decomp_avm <- function(model){
      renderPlotly({
        if(is.null(model)){
          plotly_empty(type="scatter", mode="lines")
        }
        else{
          act <- model$reg_group[[2]]
          fit <- model$get_fitted()
          cht <- plot_ly(
            x = model$reg_group[[1]],
            y = act,
            name="Actual",
            type="scatter",
            mode="lines",
            line = list(color = "black")) %>%

            add_trace(
              y = fit,
              name="Fitted",
              line = list(color = global_options$charts$colors$blue)) %>%

            add_trace(
              y = act-fit,
              name="Residuals",
              type="bar",
              marker = list(color = global_options$charts$colors$grey)) %>%

            layout(
              title = paste0(model$kpi),
              xaxis = list(
                hoverformat="%e %b %y"
              ),
              font=global_options$charts$font,
              legend = list(
                orientation = 'h',
                opacity=0.6)
            )
          return(cht)
        }
      })
    }
    render_decomp_dc <- function(model){
      renderPlotly({
        if(is.null(model)){
          plotly_empty(type="scatter", mode="lines")
        }
        else{
          act <- model$reg_group[[2]]
          decomp <- model$get_decomp()
          cht <- plot_ly(
            x = model$reg_group[[1]],
            y = act,
            name=model$kpi,
            type="scatter",
            mode="lines",
            line = list(color = "black"))

          for(i in 2:length(decomp)){
            cht <- cht %>% add_trace(
              x = model$reg_group[[1]],
              y = decomp[[i]],
              name = names(decomp)[i],
              type = "bar"
            )
          }
          cht <- cht %>% layout(
            title = paste0(model$kpi),
            xaxis = list(
              hoverformat="%e %b %y"
            ),
            barmode = 'relative',
            font=global_options$charts$font,
            legend = list(
              orientation = 'h',
              opacity=0.6)
            )
          return(cht)
        }
      })
    }
    # Decomp Download Handlers
    output$dl_model_reg <- downloadHandler(
      filename = "Regressors.csv",
      content = function(file) {
        df <- global$model$reg_group
        if(!is.null(df)){
          write_csv(df,file)
        }
      }
    )
    output$dl_model_dc <- downloadHandler(
      filename = "Decomp.csv",
      content = function(file) {
        df <- global$model$get_decomp()
        if(!is.null(df)){
          write_csv(df,file)
        }
      }
    )
  })
})
