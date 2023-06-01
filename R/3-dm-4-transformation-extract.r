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
