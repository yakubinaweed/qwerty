# ui_refiner_parallel.R

library(shiny)
library(shinyjs)
library(shinyWidgets)
library(bslib)

tab_refiner_parallel_ui <- tabPanel(
  title = "Parallel RefineR",
  useShinyjs(),
  tags$head(
    includeCSS("www/styles.css")
  ),
  sidebarLayout(
    sidebarPanel(
      style = "padding-right: 15px;",
      h4("Parallel RefineR Analysis"),
      p("This tab allows you to run refineR analysis on multiple age subpopulations in parallel."),
      
      fileInput(inputId = "data_file_parallel", label = "Upload Data (Excel File)", accept = c(".xlsx")),
      hr(),
      
      selectInput(inputId = "col_value_parallel", label = "Select Column for Values:", choices = c("None" = ""), selected = ""),
      selectInput(inputId = "col_age_parallel", label = "Select Column for Age:", choices = c("None" = ""), selected = ""),
      selectInput(inputId = "col_gender_parallel", label = "Select Column for Gender:", choices = c("None" = ""), selected = ""),

      hr(),
      textInput("age_subpopulations_input", "Define Age Subpopulations:", 
                value = "0-18, 19-39, 40-64, 65-100", 
                placeholder = "e.g., 0-18, 19-39, 40-64"),
      
      # Added new input for NBootstrap
      numericInput("nbootstrap_parallel", "Number of Bootstrap Iterations:", value = 1, min = 1, step = 1),
      
      numericInput("num_cores", "Number of CPU Cores:", value = 2, min = 1, max = 8, step = 1),
      
      radioButtons(inputId = "gender_choice_parallel", label = "Select Gender(s) for Analysis:", 
                   choices = c("Male" = "Male", "Female" = "Female", "Both" = "Both"), 
                   selected = "Both", inline = TRUE),

      radioButtons(inputId = "model_choice_parallel", label = "Select Transformation Model:",
                   choices = c("BoxCox" = "BoxCox", "modBoxCox" = "modBoxCox"),
                   selected = "BoxCox", inline = TRUE),
      
      actionButton("analyze_btn_parallel", "Start Parallel Analysis", class = "btn-primary"),
      actionButton("reset_btn_parallel", "Reset", class = "btn-secondary"),
      library(shiny)
library(shinyjs)
library(shinyWidgets)
library(bslib)

tab_refiner_parallel_ui <- tabPanel(
  title = "Parallel RefineR",
  useShinyjs(),
  tags$head(
    includeCSS("www/styles.css")
  ),
  sidebarLayout(
    sidebarPanel(
      style = "padding-right: 15px; min-width: 300px;",
      h3(icon("project-diagram"), "Parallel RefineR Analysis"),
      helpText("Run RefineR analysis on multiple age subpopulations in parallel to save time."),
      hr(),
      
      # Data Upload Section
      h4(icon("file-upload"), "Data Upload"),
      fileInput("data_file_parallel", "Upload Excel File", accept = ".xlsx"),
      helpText("Upload your dataset in Excel format. The app will attempt to detect relevant columns automatically."),
      
      hr(),
      
      # Column Selection
      h4(icon("table-columns"), "Column Mapping"),
      pickerInput("col_value_parallel", "Measurement Values:", choices = c("None" = ""), selected = ""),
      pickerInput("col_age_parallel", "Age:", choices = c("None" = ""), selected = ""),
      pickerInput("col_gender_parallel", "Gender:", choices = c("None" = ""), selected = ""),
      
      hr(),
      
      # Subpopulation Settings
      h4(icon("users"), "Subpopulation Settings"),
      textInput("age_subpopulations_input", "Age Ranges:",
                value = "0-18, 19-39, 40-64, 65-100",
                placeholder = "e.g. 0-18, 19-39, 40-64"),
      prettyRadioButtons("gender_choice_parallel", "Gender Selection:",
                         choices = c("Male", "Female", "Both"),
                         selected = "Both", inline = TRUE, status = "info"),
      
      hr(),
      
      # Advanced Settings in Collapsible Panel
      shinyWidgets::panel(
        heading = "Advanced Settings",
        numericInput("nbootstrap_parallel", "Bootstrap Iterations:", value = 1, min = 1, step = 1),
        numericInput("num_cores", "CPU Cores:", value = 2, min = 1, max = 8),
        prettyRadioButtons("model_choice_parallel", "Transformation Model:",
                           choices = c("BoxCox", "modBoxCox"),
                           selected = "BoxCox", inline = TRUE, status = "primary")
      ),
      
      hr(),
      
      # Action Buttons
      fluidRow(
        column(6, actionButton("analyze_btn_parallel", "🚀 Start Analysis", class = "btn-success btn-block")),
        column(6, actionButton("reset_btn_parallel", "🔄 Reset", class = "btn-warning btn-block"))
      ),
      
      uiOutput("app_message_parallel")
    ),
    mainPanel(
      h4("Results Summary"),
      tags$div(style = "max-height:300px; overflow:auto; border: 1px solid #ccc; padding: 10px;",
               verbatimTextOutput("summary_output_parallel")),
      downloadButton("download_summary", "📥 Download Summary"),
      hr(),
      
      h4("Subpopulation Plots"),
      helpText("Plots are interactive. Click to expand or download."),
      uiOutput("refiner_plots_parallel")
    )
  )
)

      uiOutput("app_message_parallel")
    ),
    mainPanel(
      h4("Parallel Analysis Results Summary"),
      verbatimTextOutput("summary_output_parallel"),
      
      div(class = "spacing-div"),
      
      h4("Individual Subpopulation Plots"),
      p("Click a plot to enlarge."),
      uiOutput("refiner_plots_parallel")
    )
  )
)