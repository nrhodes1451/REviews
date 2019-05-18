# Model Object ----
ev_model <- R6Class("ev_model",
  public = list(
    categories=NULL,
    end_date=NULL,
    fixed_vars=NULL,
    id=NULL,
    kpi=NULL,
    message=NULL,
    model_script=NULL,
    reg_group=NULL,
    start_date=NULL,
    timestamp=NULL,
    variable_mapping=NULL,

    initialize=function(
      id,
      start_date,
      end_date,
      timestamp){
        self$id <- id
        self$start_date <- start_date
        self$end_date <- end_date
        self$timestamp <- timestamp
    },

    parse_model = function(script=NULL, dataset){

      if(is.null(script) || script=="") return(FALSE)

      # Define EViews functions in R
      lag_var <- function(arr, lag=0, fill=0){
        arr <- as.data.frame(arr)
        if (lag > 0){
          return(append(rep(fill, lag), arr[1:(nrow(arr)-lag), ]))
        }
        else if (lag < 0){
          return(append(arr[-1:lag, ], rep(fill,-lag)))
        }
        else return(arr)
      }
      adstock_var <- function(arr, decay=0){
        arr <- 1*arr
        n <- length(arr)
        m <- as.matrix(embed(append(rep(NA,n-1),1:n),n))
        m <- decay^(m-1)
        m[is.na(m)] <- 0
        m %*% arr
      }
      ev_movav <- function(series, mavrng){
        series[is.na(series)] <- 0
        series <- 1*series
        series <- as.matrix(rollmean(zoo(series), mavrng, align="right", fill=0))
        series[is.na(series)] <- 0
        return(series)
      }
      ev_movavc <- function(series, mavrng){
        series[is.na(series)] <- 0
        series <- 1*series
        if(class(series) =="matrix" || class(series)=="numeric"){
          series <- c(series, rep(tail(series,1), floor(mavrng/2)))
        }
        else if(class(series) == "data.frame"){
          for(x in 1:floor(mavrng/2)){
            series <- rbind(series, tail(series,1))
          }
        }
        if(mavrng %% 2 == 1){
          series <- as.matrix(rollmean(series, mavrng, align="c", fill=NA) )
        }
        else{
          x <- zoo(lag_var(series,(mavrng/2)-1))
          y <- zoo(lag_var(series,mavrng/2))
          series <- (
            (as.matrix(rollmean(x, mavrng, align="l", fill=NA)) +
            as.matrix(rollmean(y, mavrng, align="l", fill=NA))) / 2)
          series[is.na(series)] <- series[!is.na(series)] %>% tail(1)
        }
        series <- head(series,-floor(mavrng/2))
        series[is.na(series)] <- 0
        return(series)
      }
      ev_mav <- ev_movav
      ev_mavc <- ev_movavc

      # Attempts to parse a text string as an Eviews command
      # If successful, returns as list:
      # data = regressor columns
      # mapping = variable mapping row
      parse_line = function(line){

        # Make a copy of the dataset in case we bugger it up...
        df <- dataset
        df_out <- df[1]

        # For variable mapping table
        varmap <- list(name=line,
                       variable=line,
                       adstock=0,
                       denominator=0)

        # And add EViews date functions...
        df$ev_year <- year(df$date)
        df$ev_quarter <- ceiling(month(df$date)/3)
        df$ev_month <- month(df$date)
        df$ev_week <- week(df$date)
        df$ev_day <- day(df$date)
        df$ev_day <- day(df$date)
        ev_strdate <- function(date_str){
          format(df$date, format="%d%m%y")
        }
        ev_after <- function(date_str){
          series <- 1 * (df$date >= dmy(date_str))
          series[is.na(series)] <- 0
          return(series)
        }
        ev_before <- function(date_str){
          series <- 1 * (df$date < dmy(date_str))
          series[is.na(series)] <- 0
          return(series)
        }

        # Replace @ with ev_ for the EViews functions
        # Replace single = with double == for R evaluation
        # Replace <> with != for R evaluation
        line <- tolower(line) %>%
          str_replace_all("@", "ev_") %>%
          str_replace_all("ev_sin", "sin") %>%
          str_replace_all("ev_cos", "cos") %>%
          str_replace_all("ev_datepart\\(\"ww\"\\)", "ev_week") %>%
          str_replace_all("=", "==") %>%
          str_replace_all("<==", "<=") %>%
          str_replace_all(">==", ">=") %>%
          str_replace_all("<>", "!=")

        if(line=="c"){
          df_out[1] = 1

          return(list(data=df_out, mapping=varmap))
        }
        else{
          # Regex match lags/decays/vars etc.
          matches <- str_match_all(line, "ev_[a-z]+|[a-z]{2}[a-z0-9]+(_\\d{2})?(\\(-?\\d+\\))?")[[1]]
          matches[is.na(matches)] <- ""
          matches %>% str_match_all("ev_")

          # Extract diminishing returns rate (if any)
          varmap$denominator <- str_match_all(line, "exp\\(.+\\/(\\d+(\\.\\d+)?)\\)")[[1]][2]

          if(nrow(matches)==0){
            self$message <- paste(c("<strong>Malformed command or missing variables:</strong>",
                                  line), collapse="<br>")
            return(NULL)
          }

          cmd_str <- strsplit(line,"ev_[a-z]+|[a-z]{2}[a-z0-9]+(_\\d{2})?(\\(-?\\d+\\))?")
          parse_str <- cmd_str[[1]][1]

          for(x in 1:dim(matches)[1]) {
            var <- substr(matches[x,1], 1, nchar(matches[x,1])-nchar(matches[x,3]))
            if(exists(var)){
              parse_str <- paste0(parse_str,var)
            }
            else{
              lagged <- matches[x,3] != ""
              dec <- substr(matches[x,2],nchar(matches[x,2])-1, nchar(matches[x,2]))
              dec <- suppressWarnings(as.integer(dec))
              if(!(var %in% colnames(df)) &&
                 !is.na(dec) &&
                 substr(var, 1, nchar(var)-3) %in% colnames(df)){
                df_out[as.character(x+1)] <- adstock_var(df[[substr(var, 1, nchar(var)-3)]], dec/100)

                varmap$adstock <- dec
                if(x==1) varmap$variable <- substr(var, 1, nchar(var)-3)
              }
              else{
                if(!(var %in% names(df))){
                  self$message <- paste(c("<strong>Missing variable:</strong>", var),
                                        collapse="<br>")
                  return(NULL)
                }
                df_out[as.character(x+1)] <- df[var]

                if(varmap$name==varmap$variable) varmap$variable <- var
              }
              if(lagged){
                lag <- as.integer(substr(matches[x,3], 2, nchar(matches[x,3])-1))
                df_out[as.character(x+1)] <- lag_var(df_out[as.character(x+1)], -lag)
              }
              parse_str <- paste0(parse_str,"df_out['", as.character(x+1), "']")
            }
            if(x < length(cmd_str[[1]])) parse_str <- paste0(parse_str,cmd_str[[1]][x+1])
          }

          parsed <- tryCatch({
            eval(parse(text=tolower(parse_str)))
          },
          # Raise errors for malformed commands
          error=function(cond){
            self$message <- paste(c("<strong>Malformed command:</strong>", line), collapse="<br>")
            self$message <- paste(c(self$message, "", "<br><strong>Message:</strong><br>", cond), collapse="<br>")
            return(NULL)
          })

          if(sum(is.na(parsed))>0){
            # Try replacing NaN values with 0
            parsed <- unlist(lapply(parsed, function(x){ifelse(is.nan(x),0,x)}))
            # If there are still NAs...
            if(sum(is.na(parsed))>0){
              self$message <- paste(c("Null values found in variable:", line), collapse="<br>")
            }
          }

          # Ignore nulls / nas
          if(is.null(parsed) || sum(is.na(parsed))>0){
            return(NULL)
          }
          else{
            return(list(data=parsed, mapping=varmap))
          }
        }
      }

      self$message <- NULL

      model_script <- strsplit(tolower(script), "\\n")[[1]]
      comments <- grep("#", model_script)
      if(length(comments)>0) model_script <- model_script[-comments]
      model_script <- strsplit(model_script, "\\s+") %>% unlist
      model_script <- model_script[nchar(model_script)>0]
      model_script <- model_script[!duplicated(model_script)]

      # Attempt to parse the KPI
      # Linear combinations of variables are accepted
      # Adstocks/diminishing returns are not accepted
      parsed_kpi <- parse_line(model_script[1])
      if(is.null(parsed_kpi)){
        self$message <- paste(c("<strong>KPI parsing failure:</strong>", model_script[1],
                                self$message), collapse="<br>")
        return(FALSE)
      }
      else if(parsed_kpi$mapping$adstock != 0){
        self$message <- paste(c("<strong>KPI parsing failure:</strong>",
                                "Adstocked dependent variables are currently unsupported."),
                              collapse="<br>")
        return(FALSE)
      }
      else if(!is.na(parsed_kpi$mapping$denominator)){
        self$message <- paste(c("<strong>KPI parsing failure:</strong>",
                                "Transformed dependent variables are currently unsupported."),
                              collapse="<br>")
        return(FALSE)
      }

      # Extract KPI whilst handling fixed variables
      # Currently only handles a single fixed variable
      ## n.b. we really shouldn't be fixing variables in our models regardless
      kpi <- model_script[1]

      self$kpi <- strsplit(kpi, "\\+|-") %>% unlist

      if(length(self$kpi)>1){
        signs <- str_match_all(kpi, "-|\\+") %>% unlist
        kpi <- self$kpi
        self$kpi <- kpi[1]

        self$fixed_vars <- dataset$date

        for(x in 1:length(signs)){
          fixed_vars <- parse_line(kpi[x+1])
          if(signs[x]=="+"){
            self$fixed_vars <- self$fixed_vars %>% cbind(-fixed_vars$data)
            names(self$fixed_vars)[x+1] <- paste0("-",kpi[x+1])
          }
          else{
            self$fixed_vars <- self$fixed_vars %>% cbind(fixed_vars$data)
            names(self$fixed_vars)[x+1] <- kpi[x+1]
          }
        }

        names(self$fixed_vars)[1] <- "date"
        self$fixed_vars[is.na(self$fixed_vars)] <- 0
        self$fixed_vars[-1] <- lapply(self$fixed_vars[-1], as.numeric)
        self$fixed_vars <- filter(self$fixed_vars,
          date>=self$start_date & date<=self$end_date)
      }

      if(tolower(self$kpi) == 'c'){
        self$message <- "Are you sure you want to regress against the constant?"
        return(NULL)
      }
      # Check if the constant is missing
      else if(!('c' %in% model_script)){
        model_script <- c(model_script[1],'c',model_script[-1])
      }

      if(is.null(model_script) ||
         is.null(dataset) ||
         is.null(dataset$date)) return(NULL)
      else{

        self$reg_group <- dataset['date']
        self$reg_group[self$kpi] <- dataset[tolower(self$kpi)]
        self$variable_mapping=NULL

        for(l in model_script){
          parsed <- parse_line(l)
          self$reg_group[l] <- parsed$data
          if(l!=model_script[1]){
            self$variable_mapping <- rbind(self$variable_mapping, parsed$mapping)
          }
        }

        # Add fixed vars to mapping table
        self$variable_mapping <- self$variable_mapping %>%
          data.frame %>%
          rbind(tail(names(self$fixed_vars),-1))

        modelid <- self$id

        self$variable_mapping <- self$variable_mapping %>%
          mutate(name=as.character(name),
                 variable = gsub("ev_", "@", as.character(variable)),
                 adstock = as.numeric(adstock),
                 denominator= as.numeric(denominator),
                 model = modelid)

        self$variable_mapping[is.na(self$variable_mapping)] <- 0

        self$reg_group[is.na(self$reg_group)] <- 0
        self$reg_group[-1] <- lapply(self$reg_group[-1], as.numeric)
        self$reg_group <- filter(self$reg_group,
          date>=self$start_date & date<=self$end_date)

        # Check all values are defined
        if(sum(!(is.finite(colSums(self$reg_group[-1]))))>0){
          self$message <- paste(c("<strong>Undefined variables:</strong>",
            names(self$reg_group[-1])[!(is.finite(colSums(self$reg_group[-1])))]),
            collapse="<br>")
        }

      }

      if(!is.null(self$message)){
        self$reg_group <- NULL
        return(FALSE)
      }
      else{
        self$model_script <- model_script
        return(TRUE)
      }
    },

    run_regression = function(){
      if(is.null(self$reg_group)) return(NULL)
      if(!is.null(self$fixed_vars)){
        private$model <- lm(self$reg_group[[3]] ~ ., data=self$reg_group[-(1:3)])
      }
      else{
        private$model <- lm(self$reg_group[[2]] ~ ., data=self$reg_group[-(1:2)])
      }
    },

    get_model = function(){
      self$run_regression()
      if(!is.null(self$fixed_vars)){
        model <- private$model %>% tidy %>% mutate(term=names(self$reg_group)[-(1:3)])
      }
      else model <- private$model %>% tidy %>% mutate(term=names(self$reg_group)[-(1:2)])
      model <- merge(self$variable_mapping, model,
                     by.x="name", by.y="term", all=T, sort=F)
      model[is.na(model)] <- 1
      names(model) <- toproper(names(model))
      model <- model %>% rename("t-statistic" = Statistic,
                              "p-value" = `P.value`) %>%
        select(-Model)
      return(model)
    },

    get_fitted = function(){
      if(!is.null(self$reg_group)){
        coeffs <- self$get_model() %>% select(Estimate)
        decomp <- self$reg_group[,-(1:(2+!is.null(self$fixed_vars)))]
        if(!is.null(self$fixed_vars)){
          decomp <- cbind(decomp, self$fixed_vars[-1])
        }
        decomp <- as.data.frame(mapply(`*`, decomp, t(coeffs)))
        return(rowSums(decomp))
      }
      else return(NULL)
    },

    get_decomp = function(){
      coeffs <- self$get_model() %>% select(Estimate)
      decomp <- self$reg_group[,-(1:(2+!is.null(self$fixed_vars)))]
      if(!is.null(self$fixed_vars)){
        decomp <- cbind(decomp, self$fixed_vars[-1])
      }
      decomp <- as.data.frame(mapply(`*`, decomp, t(coeffs)))
      return(cbind(self$reg_group[1], decomp))
    },

    get_diagnostics = function(){
      if(!is.null(private$model)){
        diag <- glance(private$model) %>% as.list
        diag$DW <- tryCatch(dwtest(private$model)$statistic,
                            error = function(e) return(NA))
        diag$BP <- tryCatch(bptest(private$model)$statistic,
                            error = function(e) return(NA))
        diag$ADF <- tryCatch(adf.test(self$reg_group[[2]])$statistic,
                            error = function(e) return(NA))
        #diagnostics$VIF <- vif(private$model)
        diag <- diag[c("r.squared",
                      "adj.r.squared",
                      "DW",
                      "BP",
                      "ADF")]
        return(diag)
      }
      else return(NULL)
    }
  ),

  private = list(
    model=NULL
  )
)

global$model <- ev_model$new(
  "model",
  min(global$model_data$date),
  max(global$model_data$date),
  now()
)