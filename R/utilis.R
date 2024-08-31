#' Get the string after a specific pattern (generic)
#'
#' This function extracts the string after a specific pattern.
#' @param string A character vector with 1 element.
#' @param pattern Pattern, string or special characters.
#' @param position A number. Which one to use, if multiple matches were found. The default is `1`. If the last one needs to be chosen but the position can range from x to y, then choose a number bigger than y and the function will choose the last one.
#' @param include Logical value. Whether to include the pattern, the default is `FALSE`.
#' @return A character vector after the pattern.
#' @usage
#' str_after_gen(string, pattern, position = 1, include = FALSE)
#' @examples
#' str_after_gen(c("John|09-01-22"),pattern = "\\|")
#' @export

str_after_gen <- function(string, pattern, position = 1, include = FALSE) {

if(!grepl(pattern,string)) {
  warning("No pattern is found, returning the whole string.")
}
  pos <- gregexpr(pattern, string) %>%
    unlist()

  if (position > length(pos)) {pos = max(pos)} else {pos = pos[position]}

if (!include) {

  pattern_digit <- nchar(pattern)

  str_tar <- substr(string,(pos + pattern_digit),nchar(string))

 }  else {
  str_tar <- substr(string,pos,nchar(string))
}
  return(str_tar)
}


#' Get the string before a specific pattern (generic)
#'
#' This function extracts the string before a specific pattern.
#' @param string A character vector.
#' @param pattern Pattern, string or special characters.
#' @param position A number. Which one to use, if multiple matches were found. The default is `1`. If the last one needs to be chosen but the position can range from x to y, then choose a number bigger than y and the function will choose the last one.
#' @param include logical value. Whether to include the pattern, the default is `FALSE`.
#' @return A character vector after the pattern.
#' @usage
#' str_before_gen(string, pattern, position = 1, include = FALSE)
#' @examples
#' str_before_gen(c("John|09-01-22"),pattern = "\\|")
#' @export


str_before_gen <- function(string, pattern, position = 1, include = FALSE) {

  if(!grepl(pattern,string)) {
    warning("No pattern is found, returning the whole string.")
    return(string)}

  pos <- gregexpr(pattern, string) %>%
    unlist()

  if (position > length(pos)) {pos = max(pos)} else {pos = pos[position]}

   if (!include){

     pattern_digit <- nchar(pattern)

    str_tar <- substr(string,1,(pos-pattern_digit))
  }  else {
    str_tar <- substr(string,1,pos)
  }

  return(str_tar)
}

#' Replace the strings with NAs
#'
#' This function identifies NA using the strings input.
#' @param x A vector.
#' @param na_string A character vector. It could contain any value that should be regarded as NA. The default is `c("NA",""," ","N/A")`
#' @return A vector with NAs created by replacing the na_string values with NA.
#' @usage
#'  na_replace(x,na_string = c("NA",""," ","N/A"))
#' @examples
#'  na_replace(mtcars$carb, na_string = 2)
#' @export

na_replace <- function(x, na_string = c("NA",""," ","N/A")) {

  x[as.character(x) %in% na_string] <- NA
  return(x)
}


#' Helper function to create a summary table for the `auto_check()`
#'
#' This function creates the unique count, missing count and non-missing count values in a data frame for a vector.
#' @param x A vector.
#' @param value A vector of summary options. It includes unique count, missing count and non-missing count values. The default is `c("unique","missing","non_missing")`
#' @return A data frame with summary information about vector x.
#' @usage
#' value_count(x, value = c("unique","missing","non_missing"))
#' @examples
#' value_count(mtcars$mpg)
#' value_count(mtcars$mpg,value = "non_missing")
#' @export

value_count <- function(x, value = c("unique","missing","non_missing")){

data.frame(unique = length(unique(x)),
           missing = sum(is.na(x)),
           non_missing = sum(!is.na(x)),stringsAsFactors = F)[value]
}

#' Helper function to check if a package is available
#'
#' @param pkg_name Package name to check
#' @return A logical value whether a package is available.
#' @usage
#' is_package_installed(pkg_name)
#' @examples
#' is_package_installed("ggplot2")
#' @export
#'
is_package_installed <- function(pkg_name) {
  if (pkg_name %in% rownames(installed.packages())) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

#' Helper function to load a package if available and throw a warning when it is not
#'
#' @param pkg_name Package to check
#'
#' @examples
#' #load_pkg(pkg_name = "tidyverse")
#'
load_pkg <- function(pkg_name) {
  pkg_availability <- base::require(pkg_name,character.only = T,quietly = T)
  if (!pkg_availability) {
    stop(sprintf("there is no package called '%s', please install '%s' or switch to a different method",pkg_name,pkg_name))
    } else {
    message(paste0("Package '", pkg_name, "' is now loaded."))
  }
}


