library(shiny)
library(reactable)
library(dplyr)
library(readr)
library(purrr)

local_file <- "mtcars.csv"

generateInput <- function(id, label, value) {
  switch(
    class(value),
    numeric = numericInput(id, label, value),
    integer = numericInput(id, label, value),
    character = if(nchar(value) > 100) {textAreaInput(id, label, value)} else {textInput(id,label, value)},
    factor = selectInput(id, label, choices = levels(value), selected = value[1]),
    logical = checkboxInput(id, label, value),
    Date = dateInput(id, label, value),
    stop("Unsupported data type")
  )
}

data_input <- function(data) {
  map2(names(data), data, ~generateInput(.x, .x, .y[1]))
}


ui <- fluidPage(
  reactableOutput("table")
)

server <- function(input, output, session) {
  data <- reactiveVal(
    read_csv(local_file)
  )

  output$table <- renderReactable({
    reactable(data(), onClick = "select", selection = "single")
  })

  selected <- reactive(getReactableState("table", "selected"))


  observeEvent(req(selected()), {
    showModal(modalDialog(
      title = "Edit row",
      data_input(data()),
      easyClose = TRUE,
      footer = tagList(
        modalButton("Cancel"),
        actionButton("delete","Delete"),
        actionButton("add","Add"),
        actionButton("update", "Update")
      )
    ))
  })

  observeEvent(input$add, {
    df <- data()
    added <- map(names(df), ~input[[.x]])
    names(added) <- names(df)
    df <- rbind(data(), data.frame(added))
    write_csv(df, local_file)
    data(df)
    removeModal()

  })

  observeEvent(input$update, {
    df <- data()
    print(map(names(df), ~input[[.x]]) )
    df[selected(),] <- map(names(df), ~input[[.x]])
    write_csv(df, local_file)
    data(df)
    removeModal()
  })

  observeEvent(input$delete, {
    df <- data()
    df <- df[-selected(), ]
    write_csv(df, local_file)
    data(df)
    removeModal()
  })

}

shinyApp(ui, server)


# progression without actions-------------------------------------------------------------

library(shiny)

local_file <- "data/mtcars.csv"

ui <- fluidPage( reactableOutput("table") )

server <- function(input, output, session) {

  data <- reactiveVal(  read_csv(local_file)  )

  output$table <- renderReactable({
    reactable(data(), onClick = "select", selection = "single")
  })

  selected <- reactive(getReactableState("table", "selected"))

  observeEvent(req(selected()), {
    showModal(modalDialog(
      title = "Edit row",
      data_input(data()),
      easyClose = TRUE,
      footer = tagList(
        modalButton("Cancel"),
        actionButton("delete","Delete"),
        actionButton("add","Add"),
        actionButton("update", "Update")
      )
    ))
  })
}

shinyApp(ui, server)


