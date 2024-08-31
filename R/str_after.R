#' Get the string after a specific pattern
#'
#' This function extracts the string after a specific pattern.
#' @include utilis.R
#' @param string A character vector. It can have multiple elements.
#' @param pattern Pattern, string or special characters.
#' @param position A number. Which one to use, if multiple matches were found. The default is `position = 1`. If the last one needs to be chosen but the position can range from x to y, then choose a number bigger than y and the function will choose the last one.
#' @param include Logical value. Whether to include the pattern, the default is `include = FALSE`.
#' @return A character vector after the pattern.
#' @usage
#' str_after(string, pattern, position = 1,include = FALSE)
#' @examples
#' str_after(c("John|09-01-22","Jane|09-02-22"),pattern = "\\|")
#' @export

str_after <- function(string, pattern, position = 1, include = FALSE) {

  if (length(string) > 1) {

    str_tar <- string %>%
     purrr::map_chr(str_after_gen,pattern = pattern,position = position, include = include)

  } else {

    str_tar <- str_after_gen(string = string, pattern = pattern,position = position, include = include)

  }

  return(str_tar)

}
