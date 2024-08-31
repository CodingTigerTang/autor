#' Simulated product usage data
#'
#' A simulated data for daily product usage from 8 customers across 4 regions from July 2021 to July 2023
#'
#' @format ## `product_use`
#' A data frame with 5,848 rows and 4 columns:
#' \describe{
#'   \item{customers}{Customer symbols}
#'   \item{date}{Date}
#'   \item{usage}{Numeric value representing the product usage}
#'   \item{region}{north, east, west, south}
#'   ...
#' }
"product_use"

# library(tibble)
# library(lubridate)
# library(readr)
# library(dplyr)
#
# set.seed(1214)
# product_use <- as_tibble(
#   list(
#     customers = rep(c(letters[1:8]),each = 731),
#     date = rep((as_date("2023-07-21"):as_date("2021-07-21")) %>% as_date(),times = 8),
#     usage = rnorm(5848, mean = 260, sd = 128),#sample(c(10:110,50:60),5848,replace = T),#rnorm(5848, mean = 300, sd = 88),
#     region = rep(c("north","east","west","south"),times = 1462)
#   )
# ) %>% mutate(usage = ifelse(usage < 0, 0, usage))
#
# usethis::use_data(product_use,overwrite = T)


#' Simulated ticket sales data
#'
#' A simulated data for daily theater ticket sales from 100 theaters across 50 states from April 2022 to Aug 2023.
#'
#' @format ## `ticket_sales`
#' A data frame with 50,900 rows and 4 columns:
#' \describe{
#'   \item{date}{Date}
#'   \item{theater}{Different theaters}
#'   \item{state}{States that the theater is located}
#'   \item{sales}{Daily sales}
#'   ...
#' }
"ticket_sales"


# library(dplyr)
# library(lubridate)
# # Set the seed for reproducibility
# set.seed(123)
#
# # Generate a list of movie theaters and states
# movie_theaters <- paste0("Theater", 1:100)
# states <- rep(state.name, length.out = 100)
#
# # Generate random data for daily ticket sales
# start_date <- ymd("2022-04-01")
# end_date <- ymd("2023-08-22")
# date_range <- seq(start_date, end_date, by = "day")
# num_days <- length(date_range)
# num_theaters <- length(movie_theaters)
#
# ticket_sales <- data.frame(
#   date = rep(date_range, times = num_theaters),
#   theater = rep(movie_theaters, each = num_days),
#   state = rep(states, each = num_days),
#   sales = sample(0:100, num_days * num_theaters, replace = TRUE)
# )
#
# usethis::use_data(ticket_sales, overwrite = T)

