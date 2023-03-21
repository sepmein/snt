#' Replace routine database
#'
#' @param df Target dataframe to be replaced
#' @export
routine_replace <- function(df) {
  country <- config_get_country()
  # filter tibble by country name
  filtered_routine_rename_database <-
    snt::import_routine_replace |>
    dplyr::filter(country == country)
  find_and_rename <- function(names) {
    from <- filtered_routine_rename_database$from
    to <- filtered_routine_rename_database$to
    formatted <- c()
    for (name in names) {
      result <- name
      for (i in seq_along(from)) {
        if (is.na(name)) {
          break
        }
        if (stringr::str_detect(name, from[i])) {
          result <- stringr::str_replace(name, from[i], to[i])
          break
        }
      }
      formatted <- append(formatted, result)
    }
    return(formatted)
  }
  df <- df |>
    dplyr::mutate_if(
      is.character,
      find_and_rename
    )
  return(df)
}

#' A helper function to import routine database
#'
#' @param df Target dataframe to be replaced
#' @param replace_database Db which stored the corrected information, usually
#' snt internal database could be used.
#' @param snt_country Target snt country, as there is multiple country replacement
#' store inside the replace db. So designate a database is necessary
#' @param column Target column in the target dataframe to be replaced
#' @author Chunzhe ZHANG
#' @export
#' @importFrom rlang .data
#' @import dplyr
import_replace <- function(df,
                           replace_database,
                           snt_country,
                           column,
                           by = FALSE) {
  temp_column <- paste0(column, "_")
  filtered_replace_database <- replace_database |>
    filter(.data$country == snt_country) |>
    select(-.data$country) |>
    rename(
      !!column := .data$from,
      !!temp_column := .data$to
    )
  if (isFALSE(by)) {
    df <- df |>
      left_join(filtered_replace_database)
  } else if (is.vector(by)) {
    by[column] <- column
    df <- df |>
      left_join(filtered_replace_database,
        by = by
      )
  }
  df <- df |>
    mutate_at(
      c(temp_column),
      ~ if_else(is.na(.x), !!sym(column), .x)
    ) |>
    select(-!!column) |>
    dplyr::rename(!!column := !!temp_column)

  return(df)
}
