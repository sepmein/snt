#' Calculate the expected reports
#' @param df A dataframe
expected <- function(df) {
  target_list <- df |>
    select(!!!args) |>
    unique()
  min_year <- min(df$year)
  max_year <- max(df$year)
  year <- tibble(year = min_year:max_year)
  month <- tibble(month = 1:13)
  target_list_ym <- target_list |>
    cross_join(year) |>
    cross_join(month)
  exp <- target_list_ym |>
    mutate(exp = 1)
  return(exp)
}
#' Calculate the reporting rate for each indicator
#'
#' For each indicator, calculate the reporting rate for each year and month
#' The reporting rate is calculated as the number of reports divided by the
#' number of expected reports. Here, the expected reports are calculated as
#' the number of unique combinations of the provided columns, multiplied by
#' the number of months and years in the data.
#' So in this function, the data is first grouped by the provided columns,
#' and then the number of unique combinations is calculated. Then, the data
#' is expanded to include all combinations of the provided columns, years,
#' and months. Then, the number of expected reports is calculated as the
#' number of unique combinations multiplied by the number of months and years.
#' Finally, the number of reports is calculated as the sum of the reports
#' for each combination of the provided columns, years, and months.
#'
#' @param df A data frame
#' @param ... Columns to calculate the reporting rate, excluding year and month
#' @return Dataframe
#' @import dplyr
#' @importFrom rlang enquos
#' @importFrom tibble tibble
#' @export
report_rate <- function(df, ...) {
  args <- enquos(...)
  rep <- df |>
    mutate(
      rep = if_else(
        rowSums(
          across(
          !c(!!!args, year, month) &
            where(is.numeric)
        ),
          na.rm = TRUE
        ) ==
          0, 0, 1
      )
    ) |>
    select(!!!args, year, month, rep)
  reprate <- exp |>
    left_join(rep) |>
    # filling NA with 0
  mutate(
    rep = if_else(
      is.na(rep),
      0, rep
    )
  )
  reprate |>
    group_by(!!!args) |>
    summarize(
      exp = sum(exp),
      rep = sum(rep)
    ) |>
    mutate(reprat = rep/exp)
}
#' Calculate the reporting rate
#'
#' The reporting rate is calculated as the number of reports divided by the
#' @param dt An aggragated dt, should already contain the meta_adm and meta_date @param adm_by A character vector of the adm columns @param date_by A character vector of the date columns
#' @param adm_by A character vector of the adm columns
#' @param date_by A character vector of the date columns
#' @return A data table, with the following columns:
#' adm_by_cols, date_by_cols, rep, exp, rep_rat
#' @importFrom data.table fifelse .SD
#' @export
sn_ana_report_status <- function(
  dt, adm_by = NULL, date_by = NULL, exclude_cols = "adm1|adm2|hf|year|month|^id",
  by_cols = NULL
) {
  # always exclude those meta columns from the dt
  exclude_cols <- grep(
    exclude_cols, names(dt),
    value = TRUE
  )
  if (is.null(by_cols)) {
    # get the by columns
    by_cols <- get_by_cols(adm_by, date_by)
  }
  report_rate <- dt[, .(
    rep = fifelse(
      sum(.SD, na.rm = TRUE) ==
        0, 0, 1
    )
  ),
    by = by_cols, .SDcols = -exclude_cols]
  return(report_rate)
}
#' report rate
#'
#' Calculate report rate
#' @export
#' @param dt data.table
#' @param adm_by character vector
#' @param date_by character vector
#' @param on 'adm' or 'index'
#' @param col character vector
#' @importFrom data.table melt .N .SD
sn_ana_report_rate <- function(
  dt, adm_by = NULL, date_by = NULL, on, col = NULL, exclude_cols = "adm1|adm2|hf|year|month|^id",
  by_cols = NULL
) {
  stopifnot(on %in% c("adm", "index"))
  # exclude those meta columns from the dt
  exclude_cols <- grep(
    exclude_cols, names(dt),
    value = TRUE
  )
  # get the by columns
  if (is.null(by_cols)) {
    by_cols <- get_by_cols(adm_by, date_by)
  }
  # calculate the expected reports
  measure.vars <- setdiff(
    names(dt),
    exclude_cols
  )
  if (!is.null(col)) {
    # get the intersect of set of mesure.vars and col
    measure.vars <- intersect(measure.vars, col)
  }
  if (on == "index") {
    # if on index pivot the dt to long format by the
    # by_cols for each indicator for each row, which is
    # meta - indicator combination, the expected value
    # should be 1 the reported values is 0 if is is an NA,
    # else it is 1
    melt_dt <- melt(dt, id.vars = by_cols, measure.vars = measure.vars)
    # then group by the by_cols, plus the pivot index
    # column and calculate the sum of expected and reported
    melt_dt[, `:=`(
      rep = !is.na(value),
      exp = 1
    )]
    rep_rat <- melt_dt[, .(
      rep = sum(rep),
      exp = sum(exp)
    ),
      by = c(by_cols, "variable")]
  } else if (on == "adm") {
    # if col is not null, then only calculate the reporting
    # rate for those columns first test those cols, they
    # should be in names(dt), otherwise stop
    if (!is.null(col)) {
      if (!all(col %in% names(dt))) {
        stop("The col argument should be a subset of names(dt)")
      }
      rep_rat <- dt[, .(
        rep = sum(
          !is.na(.SD),
          na.rm = TRUE
        ),
        exp = .N * length(.SD)
      ),
        by = by_cols, .SDcols = col]
    } else {
      # if on adm sum across the by_cols, and then
      # calculate the reporting rate
      rep_rat <- dt[, .(
        rep = sum(
          !is.na(.SD),
          na.rm = TRUE
        ),
        exp = .N * length(.SD)
      ),
        by = by_cols, .SDcols = -exclude_cols]
    }
  }
  rep_rat[, rep_rat := rep/exp]
  # after that calculate the reporting rate as the sum of
  # reported divided by the sum of expected
  return(rep_rat)
}
#' Get group by columns
#'
#' adm_by should be either adm1, adm2, or hf
#' date_by should be either year or month
#' if not, should be an error
#' if adm_by is hf, them the result should contain adm1, adm2, and hf
#' if adm_by is adm2, then the result should contain adm1 and adm2
#' if adm_by is adm1, then the result should contain adm1
#' if date_by is year, then the result should contain year
#' if date_by is month, then the result should contain year and month
#' @param adm_by The administrative level to group by, should be either adm1, adm2, or hf
#' @param date_by The date level to group by, should be either year or month
#' @return A vector of column names
#' @export
get_by_cols <- function(adm_by, date_by) {
  stopifnot(adm_by %in% c("adm1", "adm2", "hf"))
  stopifnot(date_by %in% c("year", "month"))
  if (adm_by == "hf") {
    adm_by_cols <- c("adm1", "adm2", "hf")
  } else if (adm_by == "adm2") {
    adm_by_cols <- c("adm1", "adm2")
  } else if (adm_by == "adm1") {
    adm_by_cols <- c("adm1")
  }
  if (date_by == "year") {
    date_by_cols <- c("year")
  } else if (date_by == "month") {
    date_by_cols <- c("year", "month")
  }
  by_cols <- c(adm_by_cols, date_by_cols)
  return(by_cols)
}
#' Calculate the report status
#'
#' If any indicator is reported, the report status is 'Y', otherwise 'N'
#' grouped by the provided columns
#' @param df A data frame
#' @param ... Columns to group by
#' @return Dataframe
#' @import dplyr
#' @importFrom rlang enquos .data
#' @export
report_status <- function(df, ...) {
  # create a list of arguments
  args <- enquos(...)
  # group data frame by arguments
  df <- df |>
    group_by(!!!args) |>
    # calculate sum for each group for all numeric columns
  summarise(
    across(
      !any_of(c("year", "month")) &
        where(is.numeric),
      ~sum(.x, na.rm = TRUE)
    )
  ) |>
    # ungroup data frame
  ungroup()
  df <- df |>
    mutate(
      reported = if_else(
        rowSums(
          across(
          !any_of(c("year", "month")) &
            !c(!!!args) &
            where(is.numeric)
        ),
          na.rm = TRUE
        ) ==
          0, 0, 1
      )
    )
  # select only arguments and reported column
  df |>
    select(!!!args, "reported") |>
    # convert reported to character
  mutate(reported = if_else(.data$reported > 0, "Y", "N"))
}
#' Calculate the report duration
#' Get the min max report year and month for each group based on the report status
#' This code filters the data by the reported column to only include data
#' that is reported, then groups the data by the variables specified in the
#' function call. It then calculates the min and max year and month for each
#' group, and then filters out any groups that have NA for the min year.
#' @param df A data frame with report status generated by report_status()
#' @param ... Columns to group by
#' @return Dataframe
#' @import dplyr
#' @importFrom rlang enquos
#' @export
get_report_duration <- function(df, ...) {
  args <- enquos(...)
  report_duration <- df |>
    filter(.data$reported == "Y") |>
    group_by(!!!args) |>
    summarise(
      min_year = min(.data$year, na.rm = TRUE),
      min_month = min(.data$month, na.rm = TRUE),
      max_year = max(.data$year, na.rm = TRUE),
      max_month = max(.data$month, na.rm = TRUE)
    ) |>
    ungroup() |>
    mutate(
      min_year = as.integer(.data$min_year),
      min_month = as.integer(.data$min_month),
      max_year = as.integer(.data$max_year),
      max_month = as.integer(.data$max_month)
    )
  # need the full list of groups
  df |>
    select(!!!args) |>
    distinct(!!!args) |>
    left_join(report_duration)
}
#' Get the unique values not in compare database
#'
#' This function takes a target and compare data.table and returns the unique
#' values in the target that are not in the compare.
#' @param target A data.table
#' @param compare A data.table
#' @param by A character vector of column names, should not be a list.
#' @return A data.table
#' @export
#' @examples
#' library(data.table)
#' target <- data.table(
#'  adm1 = c('a', 'a', 'b', 'b', 'c', 'c'),
#'  adm2 = c('a', 'b', 'a', 'b', 'a', 'b'),
#' hf = c('a', 'b', 'a', 'b', 'a', 'b')
#' )
#' compare <- data.table(
#'  adm1 = c('a', 'a', 'b', 'b', 'c'),
#'  adm2 = c('a', 'b', 'a', 'b', 'a'),
#' hf = c('a', 'b', 'a', 'b', 'a')
#' )
#' by <- 'adm1'
#' sn_unique_diff(target, compare, by)
sn_unique_diff <- function(target, compare, by) {
  # target should be a data.table compare should be a
  # data.table by should be a character vector of column
  # names
  tar <- target[, .SD, .SDcols = by] |>
    unique()
  comp <- compare[, .SD, .SDcols = by] |>
    unique()
  not_in_compare <- tar[!comp, on = by]
  return(not_in_compare)
}
