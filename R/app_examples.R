#' Launch the Shiny Metrics App
#'
#' This function launches the Shiny application.
#' @export
shiny_metrics <- function() {
  app_dir <- system.file("shiny_metrics/", package = "autor")
  if (requireNamespace("shiny", quietly = TRUE)) {
    shiny::runApp(app_dir, display.mode = "normal")
  }

}

#' Launch the Shiny Labeling App
#'
#' This function launches the Shiny application.
#' @export
shiny_label <- function() {
  app_dir <- system.file("shiny_label/", package = "autor")
  if (requireNamespace("shiny", quietly = TRUE)) {
    shiny::runApp(app_dir, display.mode = "normal")
  }
}

#' Launch the Shiny Editable App
#'
#' This function launches the Shiny application.
#' @export
shiny_editable <- function() {
  app_dir <- system.file("shiny_editable/", package = "autor")
  if (requireNamespace("shiny", quietly = TRUE)) {
    shiny::runApp(app_dir, display.mode = "normal")
  }
}


#' Launch the Shiny Plotting App
#'
#' This function launches the Shiny application.
#' @export
shiny_plot <- function() {
  app_dir <- system.file("shiny_plot/", package = "autor")
  if (requireNamespace("shiny", quietly = TRUE)) {
    shiny::runApp(app_dir, display.mode = "normal")
  }
}
