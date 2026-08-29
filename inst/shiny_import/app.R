library(shiny)
library(shinydashboard)
library(DT)
library(tidyverse)

# Function to summarize a single column
summarize_column <- function(column) {
  if (is.character(column)) {
    value_summary <- paste(head(unique(column), 5), collapse = ", ")
  } else {
    value_summary <- paste(range(column, na.rm = TRUE), collapse = " - ")
  }
  value_summary
}

# Function to generate summary statistics for all columns
col_pop <- function(data) {
  summary_stats <- tibble(
    Column = names(data),
    Population = map_int(data, length),
    Uniqueness = map_int(data, ~ n_distinct(.x)),
    ValueSummary = map_chr(data, summarize_column)
  )
  summary_stats
}

# Define column plot function
col_show <- function(data, column) {
  if (is.character(data[[column]])) {
    data %>%
      count(!!sym(column)) %>%
      ggplot(aes(x = reorder(!!sym(column), n), y = n)) +
      geom_col() +
      coord_flip() +
      labs(x = column)
  } else {
    data %>%
      ggplot(aes(y = !!sym(column))) +
      geom_boxplot()
  }
}

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "Data Explore"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Data Summary", tabName = "data_summary", icon = icon("table")),
      fileInput("data_file", "Choose a CSV File", accept = ".csv"),
      sliderInput("sample_pct", "Sample Percentage:",
                  min = 0, max = 100,
                  value = 1, step = 1),
      selectInput("filter_columns", "Choose Columns to Filter", choices = NULL, multiple = TRUE),
      uiOutput("filter_ui"),
      div(style = "margin-top: 15px;margin-left: 15px;", downloadButton("download_data", "Download Data"))
    )
  ),
  dashboardBody(
    fluidRow(
      box(title = "Data Sample", status = "primary", solidHeader = TRUE, width = 12,
          DTOutput("contents")
      )
    ),
    fluidRow(
      box(title = "Summary Statistics", status = "primary", solidHeader = TRUE, width = 6,
          DTOutput("summary_table")
      ),
      box(title = "Column Chart", status = "primary", solidHeader = TRUE, width = 6,
          plotOutput("column_chart")
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {

  values <- reactiveVal(NULL)

  # Load and display the dataset
  observeEvent(input$data_file, {
    req(input$data_file)
    df <- read_csv(input$data_file$datapath)
    values(df)
    updateSelectInput(session, "filter_columns", choices = names(df))
  })

  # Generate filter UI based on selected columns
  output$filter_ui <- renderUI({
    req(values(), input$filter_columns)
    df <- values()
    selected_cols <- input$filter_columns
    map(selected_cols, function(col) {
      if (is.character(df[[col]])) {
        selectInput(inputId = paste0("filter_", col), label = col, choices = unique(df[[col]]),
                    selected = unique(df[[col]]), multiple = TRUE)
      } else if (inherits(df[[col]], c("Date", "POSIXct", "POSIXt"))) {
        dateRangeInput(inputId = paste0("filter_", col), label = col,
                       start = min(df[[col]], na.rm = TRUE),
                       end = max(df[[col]], na.rm = TRUE))
      } else {
        sliderInput(inputId = paste0("filter_", col), label = col,
                    min = min(df[[col]], na.rm = TRUE),
                    max = max(df[[col]], na.rm = TRUE),
                    value = range(df[[col]], na.rm = TRUE))
      }
    })
  })

  # Apply filters to dataset
  filtered_data <- reactive({
    req(values())
    df <- values()
    if (!is.null(input$filter_columns)) {
      walk(input$filter_columns, function(col) {
        filter_input <- input[[paste0("filter_", col)]]
        if (!is.null(filter_input)) {
          if (is.character(df[[col]])) {
            df <- df |>
              filter(!!sym(col) %in% filter_input) |>
              values()
          } else {
            df <- df |>
              filter(!!sym(col) >= filter_input[1] & !!sym(col) <= filter_input[2]) |>
              values()
          }
        }
      })
    }
    # Apply random sampling based on sample_pct slider
    sample <- nrow(values())*(input$sample_pct / 100)
    df <- values() |>
      sample_n(ceiling(sample))
    df
  })

  # Display the head of the dataset
  output$contents <- renderDT({
    req(filtered_data())
    datatable(filtered_data(), options = list(pageLength = 5),rownames = FALSE)
  })

  # Calculate and display summary statistics
  output$summary_table <- renderDT({
    req(filtered_data())
    summary_stats <- col_pop(filtered_data())
    datatable(summary_stats, selection = 'single',
              options = list(pageLength = 10),rownames = FALSE) %>%
      formatCurrency(c("Population","Uniqueness"),
                     currency = "", mark = ",", digits = 0)
  })

  # Display a chart based on the selected column
  output$column_chart <- renderPlot({
    req(filtered_data())
    selected <- input$summary_table_rows_selected
    if (length(selected) == 0) return(NULL)

    column_name <- filtered_data() %>% colnames() %>% .[selected]
    col_show(filtered_data(), column_name)
  })

  # Download filtered data
  output$download_data <- downloadHandler(
    filename = function() { "filtered_data.csv" },
    content = function(file) {
      write.csv(filtered_data(), file, row.names = FALSE)
    }
  )
}

# Run the application
shinyApp(ui = ui, server = server)
