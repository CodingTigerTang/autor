#' Split and export data into files
#'
#' This will take in any data frame, split and export them into multiple files.
#' @param data the data you want to export
#' @param folder_path the destination of the to be exported files
#' @param row_lim the number of row limit per exported file.
#' @param file_names list of names you want to give to the to be exported files. Note, they should have more unique values than the number of files exported.
#' @param func is the exporting method. The default is `func = readr::write_csv`. Note: no quote is needed and you can use any export function you have installed.
#' @return A number of files containing records from data that does not exceed the `row_lim` per file.
#' @usage
#' data_split(data, folder_path, row_lim, file_names, func = readr::write_csv)
#' @examples
#' # Enable parallel processes
#' library(furrr)
#' plan(multisession, workers = 4)
#' data_split(mtcars,tempdir(),10,file_names = letters,func = readr::write_csv)
#' \dontshow{
## R CMD check: make sure any open connections are closed afterward
#' if (!inherits(plan(), "sequential")) plan(sequential)
#' }
#' @export
#'

data_split <- function(data, folder_path, row_lim, file_names, func = readr::write_csv) {

splited_data <- split(data,((seq(nrow(data)) -1)%/%row_lim))

full_names <- paste0(folder_path,"/",file_names,".csv")

splited_data %>%
  purrr::walk2(.y = full_names[1:length(splited_data)],.f = func)

message(paste0("Splited ", sprintf("%s files", length(splited_data)),": ",paste0(paste0(file_names[1:length(splited_data)],".csv"),collapse = "|")))

}


