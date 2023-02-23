# Renaming all the variables in the dataset

#' @title Renaming the dataset to lower case
#' @description Renaming the dataset to lower case
#' @param df dataframe to be renamed
#' @export
#' @importFrom dplyr rename_with everything
#' @importFrom stringr str_to_lower
#' @examples \dontrun{
#' # rename the dataframe to lower case
#' df <- tibble::tibble(
#'     A = 1:3,
#'     B = 4:6,
#'     C = 7:9
#' )
#' df_rename <- snt::to_lower(df)
#' }
#' # result
#' # A tibble: 3 x 3
#' #   a     b     c
#' #   <int> <int> <int>
#' # 1     1     4     7
#' # 2     2     5     8
#' # 3     3     6     9
to_lower <- function(df) {
    df <- df |>
        rename_with(
            ~ str_to_lower(.x),
            .cols = everything()
        )
    return(df)
}

#' @title Rename Columns in Routine data
#'
#' @description Rename columns in routine data
#' @param df dataframe to be renamed
#' @export
#' @importFrom dplyr filter
#' @importFrom stringr str_detect str_replace
routine_rename <- function(df) {
    country <- config_get_country()
    # filter tibble by country name
    df <- df |> to_lower()
    filtered_by_country <-
        snt::import_routine_rename %>%
        filter(.data[["country"]] == country)
    find_and_rename <- function(names) {
        from <- filtered_by_country$from
        to <- filtered_by_country$to
        formatted <- c()
        for (name in names) {
            result <- name
            for (i in seq_along(from)) {
                if (str_detect(name, from[i])) {
                    result <- str_replace(name, from[i], to[i])
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

#' rename the administrative zone names
#' 
#' Detect if adm0, adm1, adm2, adm3 are in the dataframe, if they are
#' use the standard names in the snt internal database to rename them
#' @param df dataframe to be renamed
#' @export
#' @importFrom dplyr filter
#' @importFrom stringr str_detect str_replace
adm_rename <- function(df) {
    country <- config_get_country()
    # filter tibble by country name
    filtered_by_country <-
        snt::import_adm_rename %>%
        filter(.data[["country"]] == country)
    find_and_rename <- function(names) {
        from <- filtered_by_country$from
        to <- filtered_by_country$to
        formatted <- c()
        for (name in names) {
            result <- name
            for (i in seq_along(from)) {
                if (str_detect(name, from[i])) {
                    result <- str_replace(name, from[i], to[i])
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