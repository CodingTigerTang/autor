#' Check the population by column
#'
#' This function automatically checks for missing values and optionally uniqueness in a dataframe.
#' @include utilis.R
#' @importFrom purrr map_dfc
#' @param data The dataframe to be checked.
#' @param na_string A character vector specifying the strings that should be treated as missing values. Defaults to c("NA", "", " ", "N/A").
#' @param check_unique Logical indicating whether to also check for uniqueness. Defaults to FALSE.
#' @return A tibble containing the check results, including column names, counts of non-missing values, and percentages of non-missing values.
#' If `check_unique` is set to TRUE, the tibble also includes counts of unique values and percentages of unique values.
#' @examples
#' auto_check(mtcars)
#' auto_check(mtcars, check_unique = TRUE)
#' auto_check(mtcars, na_string = c("NA", 999, "N/A"))
#'
#' @export

auto_check <- function(data, na_string = c("NA",""," ","N/A"),check_unique = FALSE) {

data <- data %>%
    purrr::map_dfc(na_replace,na_string)

non_missing <- data %>%
  purrr::map(value_count,value = c("non_missing")) %>%
  unlist()

if (!check_unique) {

check_result <- tibble::as_tibble(
  list(colname = names(data),
  non_missing = non_missing,
  pct = round(non_missing/nrow(data)*100,1))
)

} else {

unique <- data %>%
  purrr::map(value_count,value = c("unique")) %>%
  unlist()

check_result <- tibble::as_tibble(
    list(colname = names(data),
    unique = unique,
    non_missing = non_missing,
    uniq_pct = round(unique/nrow(data)*100,1))
  )

}
message(paste0("auto_check finds missing value with ", paste0(na_string,collapse = "|")),".")
return(check_result)

}

