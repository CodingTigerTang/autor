#' Extract tables from a Word (.docx) document
#'
#' @description
#' `extract_docx_tables()` reads a Word document and extracts all tables as a
#' list of data frames, one per table, in document order. Internally, it uses
#' `officer::docx_summary()` to access Word content at the cell level, splits
#' tables by `doc_index`, and reshapes each table to a rectangular structure.
#'
#' @param path Path to a `.docx` file.
#'
#' @return
#' A list of data frames. Each list element corresponds to one table in the
#' document (in the order they appear). The first row of each table is used as
#' the column names.
#'
#' @details
#' `officer::docx_summary()` returns table content as individual cells. Filtering
#' to `content_type == "table cell"` returns cells from all tables in the
#' document, so multi-table documents must be split by table element (using
#' `doc_index`) before reshaping.
#'
#' The function assumes that each table has a header row. If a table does not
#' contain a header row, the first data row will be promoted to column names.
#'
#' @examples
#' \dontrun{
#' tables <- extract_docx_tables("data/quarterly_sales_update.docx")
#' key_metrics <- tables[[1]]
#' regional_breakdown <- tables[[2]]
#' }
#'
#' @importFrom rlang .data
#' @export
extract_docx_tables <- function(path) {
  doc <- officer::read_docx(path) |>
    officer::docx_summary()

  doc |>
    dplyr::filter(.data$content_type == "table cell") |>
    dplyr::select("doc_index", "row_id", "cell_id", "text") |>
    dplyr::group_by(.data$doc_index) |>
    dplyr::group_split() |>
    purrr::map(function(tbl) {
      wide <- tbl |>
        dplyr::select("row_id", "cell_id", "text") |>
        tidyr::pivot_wider(
          names_from = "cell_id",
          values_from = "text"
        ) |>
        dplyr::arrange(.data$row_id) |>
        dplyr::select(-"row_id")

      names(wide) <- as.character(unlist(wide[1, ]))
      wide[-1, ]
    })
}
