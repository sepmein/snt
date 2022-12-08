#' @export
routine_rename <- function(df, cty) {
  # filter tibble by country name
  filtered_routine_rename_database <-
    snt::import_routine_rename %>%
    dplyr::filter(country == cty)
  find_and_rename <- function(names) {
    from <- filtered_routine_rename_database$from
    to <- filtered_routine_rename_database$to
    formatted <- c()
    for (name in names) {
      result <- name
      for (i in seq_along(from)) {
        if (stringr::str_detect(name, from[i])) {
          result <- stringr::str_replace(name, from[i], to[i])
          break
        }
      }
      formatted <- append(formatted, result)
    }
    return(formatted)
  }
  df <- df %>%
    dplyr::rename_with(find_and_rename)
  return(df)
}

#' @export
routine_replace <- function(df, cty) {
  # filter tibble by country name
  filtered_routine_rename_database <-
    snt::import_routine_replace %>%
    dplyr::filter(country == cty)
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
  df <- df %>%
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
import_replace <- function(df,
                           replace_database,
                           snt_country,
                           column,
                           by = FALSE) {
  temp_column <- paste0(column, "_")
  filtered_replace_database <- replace_database |>
    filter(country == snt_country) |>
    dplyr::select(-country) |>
    dplyr::rename(
      !!column := from,
      !!temp_column := to
    )
  if (isFALSE(by)) {
    df <- df |>
      dplyr::left_join(filtered_replace_database)
  } else if (is.vector(by)){
    by[column] = column
    df <- df |>
      dplyr::left_join(filtered_replace_database,
                       by = by
      )
  }
  df <- df |>
    dplyr::mutate_at(
      c(temp_column),
      ~ if_else(is.na(.x), !!sym(column), .x)
    ) |>
    dplyr::select(-!!column) |>
    dplyr::rename(!!column := !!temp_column)

  return(df)
}

dhs_api_endpoint <- "https://api.dhsprogram.com/rest/dhs/data/"

#' @export
import_dhs_data <- function(year = 2021) {
  json_file <- RJSONIO::fromJSON(
    paste0(dhs_api_endpoint, year, ",subnational")
  )
  json_data <- lapply(json_file$Data, function(x) {
    unlist(x)
  })
  APIdata <- as.data.frame(
    do.call("rbind", json_data),
    stringsAsFactors = FALSE
  )
  return(tibble::as_tibble(APIdata))
}
