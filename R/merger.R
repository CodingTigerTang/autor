#' Merge and load multiple files in your R session
#'
#' This will identify all files in a folder that meet the criteria set and import them into you session.
#' @param folder_path the path to the folder that holds the target files
#' @param pattern  the pattern that helps identifies the target files. This will take all the regex or keywords or unique characters. The default is `pattern = ""`
#' @param func the import method of your choice. The default is `func = readr::read_delim`. Note: no quote is needed and you can use any import function you have installed, for example, `func = readr::read_csv` if you have installed `readr` package.
#' @param show_name logical value. Whether we have to have the merged file to append the filename. The default is `show_name = FALSE`
#' @param para logical value. Whether we need to utilize parallel processes with the `furrr` package to import the files faster. The default is `para = FALSE`
#' @param ... Any argument you want to pass to the import function. For example, you can pass `n_max = 100` to limit the import to be 100 lines per files if you are using import functions from `readr`.
#' @return a merged data frame for all targeted files.
#' @usage
#' merger(folder_path, pattern = "", func = readr::read_delim, show_name = FALSE, para = FALSE, ...)
#' @examples
#' folder_path <- tempdir()
#' data_split(mtcars,folder_path,10,file_names = letters)
#' merger(folder_path, pattern = "csv", show_name = TRUE)
#' # Enable parallel processes
#' library(furrr)
#' plan(multisession, workers = 4)
#' merger(folder_path, pattern = "csv", show_name = TRUE, para = TRUE)
#' \dontshow{
## R CMD check: make sure any open connections are closed afterward
#' if (!inherits(plan(), "sequential")) plan(sequential)
#' }
#' @export
#'

merger <- function(folder_path, pattern = "", func = readr::read_delim, show_name = FALSE, para = FALSE, ...){

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

