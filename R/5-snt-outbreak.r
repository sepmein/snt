#' Outbreak detection
#'
#' This function is designed for outbreak detection. The algorithm
#' is two fold. 1. Calculate the mean and standard deviation of the
#' target variable for each index and year. 2. Calculate the mean and
#' standard deviation of the target variable for each index and year
#' and month. 3. Calculate the mean and standard deviation of the
#' target variable for each index and year and month, and then
#' calculate the upper limit of the target variable for each index
#' and year and month. 4. Compare the target variable with the upper
#' limit, if the target variable is greater than the upper limit,
#' then it is an outbreak.
#' @param d The data.table
#' @param col The target variable
#' @param adm_by The columns to be used for adm level aggregation
#' @param date_by The columns to be used for date aggregation
#' @param by_cols The columns to be used for aggregation
#' @return A data.table with outbreak detection result
#' @export
sn_outbreak <- function(d, col, adm_by = NULL, date_by = NULL, by_cols = NULL) {
  if (is.null(by_cols)) {
    # get the by columns
    by_cols <- get_by_cols(adm_by, date_by)
  }
  outbreak <- d[, .(id_adm2, year, month, outbreak)]
  outbreak <- outbreak[, .(
    target_col = sum(
      get(col),
      na.rm = TRUE
    )
  ),
    by = by_cols]
  outbreak[, `:=`(
    mean = mean(target_col, na.rm = T),
    sd = sd(target_col, na.rm = T),
    median = median(target_col, na.rm = T),
    p85 = quantile(target_col, 0.85, na.rm = T)
  ),
    by = .(id_adm2, year)]
  outbreak[, mean2sd := mean + 2 * sd]
  outbreak[, outbreak_2sd := fifelse(target_col > mean2sd, 1, 0)]
  outbreak[, outbreak_85pc := fifelse(target_col > p85, 1, 0)]
  return(outbreak)
}
