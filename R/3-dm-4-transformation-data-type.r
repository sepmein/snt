#' Transmute numeric columns to the correct type
#'
#' Need more documentation
#' @param df A data frame
#' @return A data frame
#' @export
sn_to_numeric <- function(df) {
  meta <- snt::meta |>
    dplyr::filter(.data[["type"]] == "numeric") |>
    dplyr::select("variable")
  # find columns in meta that are in df
  meta <- meta[meta$variable %in% names(df),
    ]
  df |>
    dplyr::transmute(is.numeric, as.integer)
}
