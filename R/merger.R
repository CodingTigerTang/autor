#' Merge and import multiple files
#'
#' Identifies files in a specified folder that match a pattern, imports them
#' using a selected import function, and combines the resulting data frames.
#'
#' @param folder_path Path to the folder containing the files to import.
#' @param pattern Character string or regular expression used to identify the
#'   target files. The default is `""`, which matches all files.
#' @param func Function used to import each file. The default is
#'   `readr::read_delim`. Other import functions can be supplied, such as
#'   `readr::read_csv`.
#' @param show_name Logical. If `TRUE`, the source filename is added to the
#'   imported data. The default is `FALSE`.
#' @param para Logical. If `TRUE`, files are imported in parallel using
#'   `furrr::future_map()`. The default is `FALSE`.
#' @param ... Additional arguments passed to `func`. For example,
#'   `n_max = 100` can be used with compatible `readr` functions to import
#'   at most 100 rows from each file.
#'
#' @return A data frame containing the combined contents of all matched files.
#'
#' @examples
#' folder_path <- tempdir()
#' data_split(mtcars, folder_path, 10, file_names = letters)
#' merger(folder_path, pattern = "csv", show_name = TRUE)
#'
#' # Enable parallel processing
#' library(furrr)
#' plan(multisession, workers = 4)
#' merger(folder_path, pattern = "csv", show_name = TRUE, para = TRUE)
#' \dontshow{
#' # R CMD check: restore the sequential plan afterward
#' if (!inherits(plan(), "sequential")) plan(sequential)
#' }
#'
#' @export
merger <- function(
    folder_path,
    pattern = "",
    func = readr::read_delim,
    show_name = FALSE,
    para = FALSE,
    ...
) {

files <- list.files(folder_path,pattern,full.names = TRUE)

import_method <- function(path, func, show_name,...) {
  data <- func(path,...)
  if (show_name) {
  data$.file_name <- str_after(path,"/")
  data
  } else {
    data
  }
}

if (!para) {

  merged <- files %>%
    purrr::map(import_method,func,show_name,...) %>%
    purrr::reduce(dplyr::bind_rows)

} else {

  merged <- files %>%
  furrr::future_map(import_method,func,show_name,...) %>%
    purrr::reduce(dplyr::bind_rows)

}

message(paste0("Merged ", sprintf("%s files", length(files)),": ",paste0(list.files(folder_path,pattern),collapse = "|")))
return(merged)
}

