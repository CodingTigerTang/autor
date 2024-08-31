library(shiny)
library(shinydashboard)
library(DT)
library(readr)
library(purrr)

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "Text Labeling Tool"),
  dashboardSidebar(
    sidebarMenu(
      fileInput("text_file", "Choose a text file", accept = c(".csv")),
      textInput("label_options", "Label Options (comma separated)", value = "Positive,Negative,Neutral"),
      textInput("keywords_group1", "Highlight Keywords Group 1 (lightgreen)", value = "Exceptional,Attentive,Delicious,Best,Awe,Captivating,Stellar,Breathtaking"),
      textInput("keywords_group2", "Highlight Keywords Group 2 (red)", value = "Horrible,Dirty,Uncomfortable,Terrible,Malfunctioning,Disappointed,Disaster,Unprepared"),
      div(style = "margin-top: 15px;margin-left: 15px;", downloadButton("download_data", "Download Labeled Data"))
    )
  ),
  dashboardBody(
    fluidRow(
      valueBoxOutput("total_texts", width = 4),
      valueBoxOutput("labeled_texts", width = 4),
      valueBoxOutput("unlabeled_texts", width = 4)
    ),
    fluidRow(
      box(title = "Raw Data", status = "primary", solidHeader = TRUE, width = 12,
          DTOutput("raw_data_table")
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  # Reactive values to store text labels and data
  text_data <- reactiveVal(data.frame(Text = character(), Label = character(), stringsAsFactors = FALSE))

  # Load file on app start
  observe({
  if (is.null(input$text_file) & !file.exists("text_data.rds")) {
    df <- read_csv("text.csv")
    df$Text <- toupper(df$Text)
    df$Label <- NA
    text_data(df)
  }
})
  # Load and display uploaded text data
  observeEvent(input$text_file, {
    req(input$text_file)
    df <- read_csv(input$text_file$datapath)
    df$Text <- toupper(df$Text)
    df$Label <- NA
    text_data(df)
  })

  # Render raw data table with highlighted keywords and editable label dropdown
  output$raw_data_table <- renderDT({
    req(text_data())
    df <- text_data()

    # Parse label options
    label_choices <- strsplit(input$label_options, ",")[[1]]

    # Parse and highlight keyword groups
    highlight_keywords <- function(df, keywords, color) {
      keywords <- strsplit(keywords, ",")[[1]]
      df$Text <- reduce(keywords, function(text, keyword) {
        gsub(keyword, sprintf("<span style='background-color: %s;'>%s</span>",
                              color, toupper(keyword)), text, ignore.case = TRUE)
      }, .init = df$Text)
      df
    }

    # Apply highlighting
    df <- highlight_keywords(df, input$keywords_group1, "lightgreen")
    df <- highlight_keywords(df, input$keywords_group2, "red")

    datatable(df, escape = FALSE, selection = 'none', options = list(
      columnDefs = list(list(
        targets = 2,
        createdCell = JS(
          "function(td, cellData, rowData, rowIndex, colIndex) {
            var labels = ", jsonlite::toJSON(label_choices), ";
            var select = $('<select></select>', { 'class': 'form-control', 'data-row': rowIndex });
            for(var i = 0; i < labels.length; i++) {
              select.append($('<option></option>', { 'value': labels[i], 'text': labels[i] }));
            }
            $(td).empty().append(select);
            $(select).val(cellData);
            $(select).change(function() {
              var data = { row: $(this).data('row'), col: 'Label', value: $(this).val() };
              Shiny.setInputValue('raw_data_table_cell_edit', data, { priority: 'event' });
            });
          }"
        )
      ))
    ))
  })

  # Handle label dropdown edit
  observeEvent(input$raw_data_table_cell_edit, {
    info <- input$raw_data_table_cell_edit
    df <- text_data()
    df[info$row + 1, "Label"] <- info$value  # +1 because JavaScript is 0-based, R is 1-based
    text_data(df)
  })

  # Save progress periodically
  autoSave <- reactiveTimer(1000)  # Automatically save progress every 1 second
  observe({
    autoSave()
    saveRDS(text_data(), file = "text_data.rds")
  })

  # Load progress on app start
  if (file.exists("text_data.rds")) {
    text_data(readRDS("text_data.rds"))
  }

  # Display progress in value boxes
  output$total_texts <- renderValueBox({
    df <- text_data()
    total <- nrow(df)
    valueBox(total, "Total Texts", icon = icon("list"), color = "aqua")
  })

  output$labeled_texts <- renderValueBox({
    df <- text_data()
    labeled <- sum(!is.na(df$Label) & df$Label != "")
    valueBox(labeled, "Labeled Texts", icon = icon("check"), color = "green")
  })

  output$unlabeled_texts <- renderValueBox({
    df <- text_data()
    unlabeled <- sum(is.na(df$Label) | df$Label == "")
    valueBox(unlabeled, "Unlabeled Texts", icon = icon("times"), color = "red")
  })

  # Download labeled data
  output$download_data <- downloadHandler(
    filename = function() {
      paste("labeled_data", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(text_data(), file, row.names = FALSE)
    }
  )
}

# Run the application
shinyApp(ui = ui, server = server)
