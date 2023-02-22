#' Remove and export missing data
#' @param df Dataframe
#' @param path Path to export missing data
#' @param ... Column to check for missing data
#' @return Dataframe
#' @importFrom dplyr filter anti_join if_any write_csv
#' @importFrom rlang enquos
#' @export
remove_and_export_missing <- function(df, path, ...) {
    args <- enquos(...)
    # get missing
    # for each column in args, if it is missing, remove the row
    missing <- df |>
        filter(if_any(c(!!!args), ~ is.na(.x)))

    # store missing data in the path
    missing |> write_csv(path)

    df <- df |>
        anti_join(missing)
}