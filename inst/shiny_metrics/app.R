library(shiny)
library(shinydashboard)
library(DT)
options(scipen=999)


ui <- dashboardPage(
  dashboardHeader(title = "Metrics Explorer", dropdownMenu(type = "messages",
                                                           messageItem(
                                                             from = "Admin",
                                                             message = "This app is created by Tiger Tang.",href = "http://tigertang.org"))),
  dashboardSidebar(
    numericInput("total_sample", "Total Sample", 10000, min = 10000),
    sliderInput("prop_actual_positives", "Proportion of Actual Positives %", 50, min = 1, max = 100, step = 1, post = "%"),
    numericInput("precision", "Precision", 0.01, min = 0.01, max = 1, step = 0.01),
    numericInput("recall", "Recall", 0.01, min = 0.01, max = 1, step = 0.01),
    textInput("use_case", "Use Case", "loan default")
  ),

  dashboardBody(
    fluidRow(
      box(
        title = "Confusion Matrix",
        status = "info",
        solidHeader = TRUE,
        collapsible = TRUE,
        width = 6,
        DTOutput("confusion_matrix")
      ),
      box(
        title = "Product Performance",
        status = "info",
        solidHeader = TRUE,
        collapsible = TRUE,
        width = 6,
        verbatimTextOutput("performance_text")
      )
    ),
    box(
      title = "Metrics Explanation",
      status = "info",
      solidHeader = TRUE,
      collapsible = TRUE,
      width = 12,
      verbatimTextOutput("metrics_explanation")
    )
  )
)

server <- function(input, output) {

  # Reactive values to store user inputs
  values <- reactiveValues(
    total_sample = 100,
    prop_actual_positives = 50,
    precision = 0.01,
    recall = 0.01,
    use_case = "loan default prediction"
  )

  # Update reactive values based on user inputs
  observe({
    values$total_sample <- input$total_sample
    values$prop_actual_positives <- input$prop_actual_positives/100
    values$precision <- input$precision
    values$recall <- input$recall
    values$use_case <- input$use_case
  })

  # Validate inputs and show alerts for negative TP, TN, FP, FN
  observe({
    tp <- round(values$prop_actual_positives * values$total_sample * values$recall)
    fn <- values$prop_actual_positives * values$total_sample - tp
    fp <- tp/values$precision - tp
    tn <- (1-values$prop_actual_positives) * values$total_sample - fp
    # print(c(tp,fn,fp,tn))


    if (tp < 0 | fp < 0 | fn < 0 | tn < 0)  {
      showModal(
        modalDialog(
          title = "Alert",
          "The given precision and recall values result in negative TP, FP, or FN. Please adjust.",
          easyClose = TRUE,
          footer = NULL
        )
      )
    }
  })

  # Calculate confusion matrix
  confusion_matrix <- reactive({

    tp <- round(values$prop_actual_positives * values$total_sample * values$recall)
    fn <- round(values$prop_actual_positives * values$total_sample - tp)
    fp <- round(tp/values$precision - tp)
    tn <- round((1-values$prop_actual_positives) * values$total_sample - fp)


    matrix(c(tp, fp, fn,tn), ncol = 2,
           dimnames = list(c("Actual Positive", "Actual Negative"),
                           c("Predicted Positive", "Predicted Negative")))
  })

  # Render minimalistic confusion matrix as DataTable
  output$confusion_matrix <- renderDT({

    cm <- confusion_matrix()

    cm[1,1] <- paste0(format(as.numeric(confusion_matrix()[1,1]),big.mark = ","), " TP")
    cm[1,2] <- paste0(format(as.numeric(confusion_matrix()[1,2]),big.mark = ","), " FN")
    cm[2,1] <- paste0(format(as.numeric(confusion_matrix()[2,1]),big.mark = ","), " FP")
    cm[2,2] <- paste0(format(as.numeric(confusion_matrix()[2,2]),big.mark = ","), " TN")

    cm %>%
      datatable(
        options = list(dom = 't', paging = FALSE, ordering = FALSE,columnDefs = list(list(className = 'dt-center', targets = 1:2)))) %>%
      formatStyle("Predicted Positive",fontWeight = "Bold",
                  color = styleEqual(c(cm[1,1],cm[2,1]),c("green","red")),
                  backgroundColor =  styleEqual(c(cm[1,1],cm[2,1]),c('#c8e6c9', '#ffcdd2'))) %>%
      formatStyle("Predicted Negative",fontWeight = "Bold",
                  color = styleEqual(c(cm[2,2],cm[1,2]),c("green","red")),
                  backgroundColor =  styleEqual(c(cm[2,2],cm[1,2]),c('#c8e6c9', '#ffcdd2')))

  })

  # Render product performance details
  output$performance_text <- renderPrint({
    cat("Successful Predictions: ", format(confusion_matrix()[1, 1], big.mark = ","), "TP (True Positives)", "\n")
    cat("Accurate Non-events: ", format(confusion_matrix()[2, 2], big.mark = ","), "TN (True Negatives)", "\n")
    cat("False Alarms: ", format(confusion_matrix()[2, 1], big.mark = ","), "FP (False Positives)", "\n")
    cat("Missed Opportunities: ", format(confusion_matrix()[1, 2], big.mark = ","), "FN (False Negatives)", "\n")

  })

  # Render metrics explanation
  output$metrics_explanation <- renderPrint({
    use_case <- tolower(values$use_case)

    cat(paste0("For every ",format(values$total_sample, big.mark = ","), " events of predicting ", use_case, ", here's what the metrics and values mean:\n\n"))

    cat("True Positives (TP):", format(confusion_matrix()[1, 1], big.mark = ","), "Successful Predictions\n")
    cat("True Negatives (TN):", format(confusion_matrix()[2, 2], big.mark = ","), "Accurate Non-events\n")
    cat("False Positives (FP):", format(confusion_matrix()[2, 1], big.mark = ","), "False Alarms\n")
    cat("False Negatives (FN):", format(confusion_matrix()[1, 2], big.mark = ","), "Missed Opportunities\n\n")

    cat("Correct Predictions (TP + TN):",
        format(confusion_matrix()[1, 1] + confusion_matrix()[2, 2], big.mark = ","), "\n")
    cat("Incorrect Predictions (FP + FN):",
        format(confusion_matrix()[1, 2] + confusion_matrix()[2, 1], big.mark = ","), "\n\n")

    # Render actual positive n negative events details
    actual_positives <- round(values$prop_actual_positives * values$total_sample)
    cat("Total Actual Positive Events: ", actual_positives %>% format(big.mark = ","), "\n")
    actual_negatives <- values$total_sample - round(values$prop_actual_positives * values$total_sample)
    cat("Total Actual Negative Events: ", actual_negatives %>% format(big.mark = ","), "\n\n")

    cat("Precision: Out of the instances predicted positive (TP + FP), what percentage actually occurred (TP)? ",
        sprintf("%.2f%%", (confusion_matrix()[1, 1] / (confusion_matrix()[1, 1] + confusion_matrix()[2, 1])) * 100), "\n")
    cat("Recall: Of all the instances that actually occurred (TP + FN), what percentage did the model correctly predict (TP)? ",
        sprintf("%.2f%%", (confusion_matrix()[1, 1] / (confusion_matrix()[1, 1] + confusion_matrix()[1, 2])) * 100), "\n")
  })

}

shinyApp(ui, server)
