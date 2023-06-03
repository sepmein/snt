#' @title Get odd or even rows from a tibble
#' @description Get odd or even rows from a tibble
#' @param df tibble
#' @param odd boolean, if TRUE then get odd rows, if FALSE then get even rows
#' @return tibble
#' @export
#' @import dplyr
sn_reduce_odd_or_even <- function(df, odd = TRUE) {
  subset <- seq_len(nrow(df))%%2
  if (odd) {
    result <- df[subset == 1, ]
  } else {
    result <- df[subset == 0, ]
  }
  return(result)
}
#' @title Extract month from year and week
#' @description Extract month from year and week
#'
#' This function is used to extract month from year and week. The reason why we
#' need this function is that `lubridate::week()` function returns the week
#'  number in the year, but not the month. For example, 2019-12-31 is in the
#' first week of 2020, but it is in December 2019. This function can help us to get the month. The result is a number between 1 and 12. 1 means January, 2 means February, and so on. The function is based on `lubridate::make_date()` and `lubridate::week()`.
#' The function will firstly create a date object with the first day of the
#' year, and then set the week of the date object to the week number we want.
#' After that, we can get the month of the date object.
#' @param year year in number
#' @param week week in number
#' @return month in number
#' @export
#' @importFrom lubridate make_date week month
#' @examples
#' sn_extract_month_from_year_week(2020, 1)
#' sn_extract_month_from_year_week(2020, 53)
sn_extract_month_from_year_week <- function(year, week) {
  date <- make_date(year = year, month = 1, day = 1)
  week(date) <- week
  return(month(date))
}
