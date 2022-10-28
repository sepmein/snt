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
