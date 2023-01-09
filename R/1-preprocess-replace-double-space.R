#' @title Replace double space
#' replace two or more spaces with one space
#' @param df dataframe
#' @param column column to replace
#' @return dataframe
#' @export
#' @importFrom dplyr mutate
#' @importFrom stringr str_replace
#' @examples
#' df <- data.frame(
#'     col = c("a  b", "b  c", "c  d")
#' )
#' replace_double_space(df, col)
replace_double_space <- function(df, column) {
    df %>%
        mutate(
            {{ column }} := str_replace({{ column }}, " +", " ")
        )
}