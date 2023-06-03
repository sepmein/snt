#' Get the intersect from two dbs
#'
#' This function is desinged for generate a data.table
#' which is the unique combination value of the columns provided
#' from two data.tables. The columns provided must be the same
#' in two data.tables.
#' @param d1 The first data.table
#' @param d2 The second data.table
#' @param col The columns to be used for intersect, must be the same
#' in two data.tables. If not provided, all columns will be used.
#' col should be a character vector, or a list of character vectors,
#' It will be passed to data.table .SDcols. Examples are provided in
#' the data.table document.
#' @return A data.table which is the unique combination value of the
#' columns provided from two data.tables.
#' @examples
#' d1 <- data.table(a = c(1, 2, 3, 4, 5),
#'                 b = c(1, 2, 3, 4, 5),
#'                 c = c(1, 2, 3, 4, 5))
#' d2 <- data.table(a = c(1, 2, 3, 4, 5),
#'                b = c(1, 2, 3, 4, 5),
#'              c = c(1, 2, 3, 4, 5))
#' sn_intersect(d1, d2, c('a', 'b'))
#' @export
sn_intersect <- function(d1, d2, col) {
  m_d1 <- d1[, .SD, .SDcols = c(col)] |>
    unique()
  m_d2 <- d2[, .SD, .SDcols = c(col)] |>
    unique()
  m_d1[m_d2, on = c(col),
    nomatch = NULL]
}
#' Get the anti-join from two dbs
#' @inheritParams sn_intersect
#' @return A data.table which is the unique combination value of the
#' columns provided from two data.tables.
#' @export
sn_antijoin <- function(d1, d2, col) {
  d1[, (col) := lapply(.SD, as.factor),
    .SDcols = c(col)]
  d2[, (col) := lapply(.SD, as.factor),
    .SDcols = c(col)]
  m_d1 <- d1[, .SD, .SDcols = c(col)] |>
    unique()
  m_d2 <- d2[, .SD, .SDcols = c(col)] |>
    unique()
  m_d1[!m_d2, on = c(col)]
}
