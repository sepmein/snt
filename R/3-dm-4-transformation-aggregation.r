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
  return(sum(data, na.rm = TRUE)/length(data))
}

#' Sum but keep NA
#'
#' NA is a special value in R, it is not equal to any other value, even itself.
#'
#' When we sum a vector with NA, the result is always NA. The reason for this
#' behavior is that NA reprensent unreported data, so we cannot make any
#' assumption about it. When aggregate data, we need to keep the NA value.
#" NA_real_" is the NA value for numeric data. We need to use it instead of NA
#' because NA is a logical value.
#' @param x a Combination of numeric values
#' @return a number
#' @export
sn_keep_na_sum <- function(x) {
  if (all(is.na(x))) {
    return(NA_real_)
  } else {
    return(sum(x, na.rm = TRUE))
  }
}
