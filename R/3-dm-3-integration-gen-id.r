#' Generate ID if not exist
#' @param df data frame
#' @param id_column ID column name
#' @return data frame
#' @author Chunzhe ZHANG
#' @export
#' @importFrom rlang :=
#' @importFrom dplyr mutate
#' @importFrom uuid UUIDgenerate
#' @importFrom dplyr n
#' @examples
#' df <- data.frame(
#'   a = c(1, 2, 3),
#'   b = c(4, 5, 6)
#' )
#' df <- gen_id_if_not_exist(df)
#' df <- gen_id_if_not_exist(df, 'id')
gen_id_if_not_exist <- function(df, id_column = "id") {
  if (!(id_column %in% colnames(df))) {
    df <- df |>
      dplyr::mutate(!!id_column := replicate(dplyr::n(), uuid::UUIDgenerate()))
  }
  return(df)
}

#' Generate ID if not exist data.table version
#' @param dt data table
#' @param id_column ID column name
#' @return data table
#' @export
#' @importFrom data.table :=
#' @importFrom uuid UUIDgenerate
sn_gen_id <- function(dt, id_column = "id") {
  if (!(id_column %in% colnames(dt))) {
    dt[, (id_column) := replicate(
      nrow(dt),
      UUIDgenerate()
    )]
  }
  return(dt)
}
