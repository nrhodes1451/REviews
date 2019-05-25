# Header
header <- dashboardHeader(title = "Modelling Dashboard")

# Sidebar ----

sidebar <- dashboardSidebar(
  hr(),

  sidebarMenu(id="tabs",
    # Input: Select a file ----
    fileInput("file1", "Load CSV File",
              multiple = FALSE,
              accept = c("text/csv",
                         "text/comma-separated-values,text/plain",
                         ".csv")),
    menuItem("Explorer", tabName = "explorer", icon = icon("search")),
    menuItem("Modelling", tabName = "modelling", icon = icon("bar-chart"))

  )
)

# Body ----

body <- dashboardBody(
  useShinyjs(),
  # HTML Tags ----
  tags$head(
    tags$script(src = "scripts.js"),
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
    tags$link(rel="shortcut icon",
    href="favicon.ico")
  ),

  # Loading bars ----
  conditionalPanel(condition="check_shiny_busy()", id="loading",
    tags$div(class="plotlybars-wrapper",
      tags$div(class="plotlybars",
        tags$div(class="plotlybars-bar b1"),
        tags$div(class="plotlybars-bar b2"),
        tags$div(class="plotlybars-bar b3"),
        tags$div(class="plotlybars-bar b4"),
        tags$div(class="plotlybars-bar b5"),
        tags$div(class="plotlybars-bar b6"),
        tags$div(class="plotlybars-bar b7")
      ),
    tags$div(class="plotlybars-text")
    )
  ),

  tabItems(
    # Error message
    htmlOutput("error_message"),
    # Data Explorer ----
    tabItem(tabName = "explorer",
      fluidRow(id="explorer_row1",
        column(width=3,
          box(
            width = NULL,
            height=380,
            status = global_options$color,
            solidHeader = TRUE,
            title=paste("Data Explorer"),
            selectInput("inp_exp_src", "Search:", NULL),
            downloadButton('dl_explorer_cht', 'Download Data'),
            plotlyOutput("render_explorer_hy", height="170px")
          )
        ),
        box(
          width = 9,
          height=380,
          status = global_options$color,
          solidHeader = TRUE,
          title="Time Series",
          plotlyOutput("render_explorer", height="325px")
        )),
      fluidRow(id="explorer_row2",
        box(
          width = 12,
          height=400,
          status = global_options$color,
          solidHeader = TRUE,
          title="Last 12 Months",
          plotlyOutput("render_explorer_latest_year", height="340px")
        ))
    ),
    # Modelling ----
    tabItem(tabName = "modelling",
      fluidRow(id="decomp_row1",
        column(width=3,
          # Model Selection
          box(
            id="decomp_models",
            width = NULL,
            status = global_options$color,
            solidHeader = TRUE,
            title="Modelling",
            collapsible= TRUE,
            selectInput("inp_decomp_model", "Model:", "unsaved model"),
            actionButton('btn_decomp_model_load', 'Load Model'),
            actionButton('btn_decomp_model_save', 'Save Model'),
            actionButton('btn_decomp_model_est', 'Estimate Model')

          )
        ),
        column(width=9,
          # Modelling textbox
          box(
            id="decomp_model_txt",
            width = NULL,
            status = global_options$color,
            solidHeader = TRUE,
            title="Console",
            collapsible= TRUE,
            tags$textarea(id="txt_decomp_eqn")
          )
        )
      ),
      # Modelling Charts ----
      fluidRow(id = "decomp_row2",
        tabBox(
          title = "Model Data",
          width = 12,
          side="right",
          tabPanel("Model Coefficients",
                   rHandsontableOutput("coeffs_table")),
          tabPanel("Model Diagnostics",
                   rHandsontableOutput("diagnostics_table"))
        )
      ),
      fluidRow(id = "decomp_row3",
        box(
          status = global_options$color,
          width = 12,
          solidHeader = TRUE,
          title = "Actual vs. Fitted",
          collapsible = TRUE,
          collapsed = TRUE,
          # Regressor group download
          downloadButton('dl_model_reg', class = 'icon-button', ''),
          plotlyOutput("render_decomp_avm",
                       height = "340px")
        )
      ),
      fluidRow(id="decomp_row4",
        box(
          status = global_options$color,
          width = 12,
          solidHeader = TRUE,
          title="Decomposition",
          collapsible = TRUE,
          collapsed = TRUE,
          # Decomp group download
          downloadButton('dl_model_dc', class = 'icon-button', ''),
          plotlyOutput("render_decomp_dc",
                       height = "340px")
        )
      )
    )
  )
)

# Run dashboard ----
dashboardPage(
  header = header,
  sidebar = sidebar,
  body = body,
  title = paste("Modelling Dashboard")
)
