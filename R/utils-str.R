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
#'   col = c("a  b", "b  c", "c  d")
#' )
#' str_trim_space(df, col)
str_trim_space <- function(df, column) {
  df |>
    mutate(
      {{ column }} := str_replace({{ column }}, " +|\t+", " ")
    )
}

#' String Util Slice a pattern and append it to the end
#'
#' @param str target string
#' @param pattern target pattern to match
#' @param concatenate optional concatenate string, default to a space
#'
#' @return string
#' @export
#'
#' @examples
#' "General Hospital Awa Onna"
#' str_slice_to_end("General Hospital Awa Onna", "General Hospital")
#' "Awa Onna General Hospital"
#' @importFrom stringr str_remove str_c str_trim str_detect
str_slice_to_end <-
  function(df, column, pattern) {
    df |>
      mutate({{ column }} := str_replace(
        {{ column }},
        str_c("(.*)(", pattern, ")(.*)"),
        "\\1 \\3 \\2"
      )) |>
      str_trim_space({{ column }}) |>
      mutate({{ column }} := str_trim({{ column }}))
  }
