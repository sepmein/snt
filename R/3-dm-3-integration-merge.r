#' Get the intersect from two dbs
#'
#' This function is designed for generate a data.table
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

#' Pre-merge checks for two dbs
#'
#' This function is designed for generate a data.table list
#' which contains three data.tables, the first one is the
#' intersect of the two data.tables, the second one is the
#' rows only in the first data.table, the third one is the
#' rows only in the second data.table. The columns provided
#' must be the same in two data.tables.
#' @return A data.table list which contains three data.tables
#' @examples
#' d1 <- data.table(a = c(1, 2, 3, 4, 5),
#'                b = c(1, 2, 3, 4, 5),
#'             c = c(1, 2, 3, 4, 5))
#' d2 <- data.table(a = c(1, 2, 3, 4, 5),
#'               b = c(1, 2, 3, 4, 5),
#'            c = c(1, 2, 3, 4, 5))
#' sn_pre_merge(d1, d2, c('a', 'b'))
#' @export
sn_pre_merge <- function(d1, d2, col) {
  # Select the specified column from d1 and remove duplicates
  m_d1 <- d1[, .SD, .SDcols = c(col)] |>
    unique()
  
  # Select the specified column from d2 and remove duplicates
  m_d2 <- d2[, .SD, .SDcols = c(col)] |>
    unique()
  
  # Join m_d1 and m_d2 on the specified column, exclude unmatched rows
  joined <- m_d1[m_d2, on = c(col),
    nomatch = NULL]
  
  # Find rows in m_d1 that are not in m_d2
  d1_only <- m_d1[!m_d2, on = c(col)]
  
  # Find rows in m_d2 that are not in m_d1
  d2_only <- m_d2[!m_d1, on = c(col)]
  
  # Combine the joined data, d1_only data, and d2_only data into a list
  result <- list(joined, d1_only, d2_only)
  
  # Return the result
  return(result)
}
