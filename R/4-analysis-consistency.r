#' @title Consistency check
#' @description Consistency check
#' @param df dataframe
#' @param index_a smaller index for comparison
#' @param index_b larger index for comparison
#' @return plot
#' @author Chunzhe ZHANG
#' @export
#' @import dplyr
#' @import ggplot2
consistency_check <- function(df, index_a, index_b) {
  df |>
    mutate(
      comparison = if_else(
        {
          {
          index_a
          }
        } < {
          {
          index_b
          }
        }, TRUE, FALSE
      )
    ) |>
    ggplot() + geom_point(
    aes(
      y = {
        {
          index_a
        }
      }, x = {
        {
          index_b
        }
      }, color = .data$comparison
    )
  ) +
    geom_abline(slope = 1) +
    theme_snt()
}
