library(shiny)
library(readr)

ui <- fluidPage(

  # App title ----
  titlePanel("Uploading Files"),

  # Sidebar layout with input and output definitions ----
  sidebarLayout(

    # Sidebar panel for inputs ----
    sidebarPanel(

      # Input: Select a file ----
      fileInput("file1", "Choose a text File",
                multiple = TRUE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),

      # Horizontal line ----
      tags$hr(),

      # Input: Checkbox if file has header ----
      checkboxInput("header", "Header", TRUE),

      # Input: Select separator ----
      radioButtons("sep", "Separator",
                   choices = c(Comma = ",",
                               Semicolon = ";",
                               Tab = "\t",
                               Pipe = "|",
                               Tilda = "~"),
                   selected = ","),
      textInput("sep2",label = "Other Separators",value = ""),

      # Input: Select quotes ----
      radioButtons("quote", "Quote",
                   choices = c(None = "",
                               "Double Quote" = '"',
                               "Single Quote" = "'"),
                   selected = '"'),

      # Horizontal line ----
      tags$hr(),

      # Input: Select number of rows to display ----
      radioButtons("disp", "Display",
                   choices = c(Head = "head",
                               All = "all"),
                   selected = "head"),
      radioButtons("data_use", h3("Data Source"),
                   choices = list("Default" = 1, "Imported" = 2),selected = 1)

    ),

    # Main panel for displaying outputs ----
    mainPanel(

      # Output: Data file ----
      dataTableOutput("contents"),

      fluidRow(uiOutput('criteria1'),uiOutput('criteria1_a')),
      fluidRow(uiOutput('criteria2'),uiOutput('criteria2_a')),
      uiOutput('action_data')
    )

  )
)


server <- function(input, output, session) {

  values <- reactiveValues(df = ggplot2::mpg)

  observeEvent(input$file1$datapath, {

    if (input$data_use == 1) {
      values$df <- read_delim(input$file1$datapath,
                              col_names = input$header,
                              delim = ifelse(input$sep2 == "",input$sep,input$sep2),
                              quote = input$quote) }
  })

  code <- reactive({
    input$filter_code
  })

  observeEvent(input$update_data, {
    data <- read_delim(input$file1$datapath,
                       col_names = input$header,
                       delim = ifelse(input$sep2 == "",input$sep,input$sep2),
                       quote = input$quote)
    if (input$filter_code == "") {
      if ( class(data[[input$filter_column]]) %in% c("numeric","integer")) {
        req(input$filter1)
        values$df <- data %>%
          filter(eval(as.name(input$filter_column)) >= min(input$filter1),
                 eval(as.name(input$filter_column)) <= max(input$filter1))
      } else if (class(data[[input$filter_column]]) %in% c("character","factor") ){
        print(1)
        req(input$filter1)
        values$df <- data %>%
          filter(eval(as.name(input$filter_column)) == input$filter1)
        print(input$filter1)
      } else {

        values$df <- data

      } } else {
        values$df <-eval(parse(text = code()) )
      }
  }
  )

  # data_upload -------------------------------------------------------------

  output$criteria1 <- renderUI({
    req(input$file1$datapath)

    div(selectInput('filter_column','filter',c("",names(values$df)),selected = ""),
        textAreaInput('filter_code','Put your filtering code',placeholder = "data"))
  })

  output$criteria1_a <- renderUI({
    req(input$filter_column)

    if ( class(values$df[[input$filter_column]]) %in% c("numeric","integer")) {

      filter_ui_1 <- sliderInput(inputId = "filter1",label = "Choose a range",min = min(values$df[[input$filter_column]]),
                                 max = max(values$df[[input$filter_column]]),value = c(min(values$df[[input$filter_column]]),max(values$df[[input$filter_column]])))

    } else if (class(values$df[[input$filter_column]]) %in% c("character","factor")) {

      filter_ui_1 <- selectInput('filter1','choose a value',c("",unique(values$df[[input$filter_column]])))
    }

    # filter_ui_1

  })


  output$action_data <- renderUI({

    req(input$file1$datapath)
    actionButton("update_data",label = "Update Data",icon = icon("sync"))

  })

  # end of data upload ------------------------------------------------------



  output$contents <- renderDataTable({


    if(input$disp == "head") {
      return(head(values$df))
    }
    else {
      return(values$df)
    }

  })

}


shinyApp(ui, server)


# latest example ----------------------------------------------------------

library(shiny)
library(readr)
library(DT)
library(tidyverse)

summarize_column <- function(column) {
  if (is.character(column)) {
    value_summary <- paste(head(unique(column), 5), collapse = ", ")
  } else {
    value_summary <- paste(range(column, na.rm = TRUE), collapse = " - ")
  }
  value_summary
}

col_pop <- function(data) {
  summary_stats <- tibble(
    Column = names(data),
    Population = map_int(data, length),
    Uniqueness = map_int(data, ~ n_distinct(.x)),
    ValueSummary = map_chr(data, summarize_column)
  )
  summary_stats
}

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

ui <- fluidPage(
  titlePanel("Uploading Files"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file1", "Choose a text File",
                multiple = TRUE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      selectInput("filter_columns", "Choose Columns to Filter", choices = NULL, multiple = TRUE),
      uiOutput("filter_ui")
    ),
    mainPanel(
      dataTableOutput("contents"),
      column(6,DTOutput("summary_table")),
      column(6,plotOutput("column_chart"))
    )
  )
)

server <- function(input, output, session) {

  values <- reactiveVal(NULL)

  observeEvent(input$file1$datapath, {
    read_delim(input$file1$datapath) |>
      values()
    updateSelectInput(session, "filter_columns", choices = names(values()))
  })

  output$filter_ui <- renderUI({
    req(values(), input$filter_columns)
    df <- values()
    selected_cols <- input$filter_columns
    map(selected_cols, function(col) {
      if (inherits(df[[col]], c("character", "logical", "factor"))) {
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
    df <- values()
    df
  })

  output$contents <- renderDataTable({
    datatable(filtered_data(),rownames = FALSE)
  })

  output$summary_table <- renderDT({
    summary_stats <- col_pop(filtered_data())
    datatable(summary_stats, selection = 'single',
              options = list(pageLength = 10),rownames = FALSE) %>%
      formatCurrency(c("Population","Uniqueness"),
                     currency = "", mark = ",", digits = 0)
  })

  output$column_chart <- renderPlot({
    selected <- input$summary_table_rows_selected
    if (length(selected) == 0) return(NULL)

    column_name <- filtered_data() %>% colnames() %>% .[selected]
    col_show(filtered_data(), column_name)
  })

}

shinyApp(ui, server)
