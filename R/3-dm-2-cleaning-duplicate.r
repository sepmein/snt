#' Check for duplicates in a data frame
#' @param df Dataframe
#' @param ... Column to check for duplicates
#' @return Dataframe
#' @importFrom dplyr group_by summarise filter n
#' @importFrom rlang enquos
#' @export
sn_check_dups <- function(df, ...) {
  args <- enquos(...)
  df |>
    group_by(!!!args) |>
    summarise(n = n()) |>
    filter(n > 1)
}

#' Merge duplicates in a data frame
#' @param df Dataframe
#' @param ... Column to check for duplicates
#' @return Dataframe
#' @importFrom dplyr group_by summarise_all ungroup anti_join bind_rows left_join select
#' @importFrom rlang enquos
#' @export
sn_clean_dups <- function(df, ...) {
  args <- enquos(...)
  # get dups
  dups <- df |>
    check_dups(...)
  # get dups to merge from df
  dups_to_merge <- dups |>
    select(!!!args) |>
    left_join(df, multiple = "all")

  # summarize dups by sum up all other columns excerpt for args
  dups_to_merge <- dups_to_merge |>
    group_by(!!!args) |>
    summarise_all(~sum(.x, na.rm = TRUE)) |>
    ungroup()

  # remove dups from df
  df <- df |>
    anti_join(dups)

  # merge dups_to_merge with df
  df <- df |>
    bind_rows(dups_to_merge)
  
  return(df)
}
