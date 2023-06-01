#' Faster Mean
#'
#' @param data a Combination of numeric values
#' @return a number
#' @export
#' @examples
#' faster_mean(c(1, 2, 3))
#' @author Spencer Zhang (sepmein@gmail.com)
#' @description
#' Detailed the reasons please refer to this
#' [answer](https://stackoverflow.com/a/18604487/886198)
sn_math_mean <- function(data) {
  return(sum(data, na.rm = TRUE) / length(data))
}
