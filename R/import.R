#' @export
routine_rename <- function(df, country) {
    # filter tibble by country name
    filtered_routine_rename_database <-
        snt::import_routine_rename %>%
        dplyr::filter(country == country)
    find_and_rename <- function(names) {
        from <- filtered_routine_rename_database$from
        to <- filtered_routine_rename_database$to
        formatted <- c()
        for (name in names) {
            result <- name
            for (i in seq_along(from)) {
                if (stringr::str_detect(name, from[i])) {
                    result <- stringr::str_replace(
                        name, from[i], to[i]
                    )
                    break
                }
            }
            formatted <- append(formatted, result)
        }
        return(formatted)
    }
    df <- df %>%
        dplyr::rename_with(
            find_and_rename
        )
    return(df)
}

#' @export
routine_replace <- function(df, country) {
    # filter tibble by country name
    filtered_routine_rename_database <-
        snt::import_routine_replace %>%
        dplyr::filter(country == country)
    find_and_rename <- function(names) {
        from <- filtered_routine_rename_database$from
        to <- filtered_routine_rename_database$to
        formatted <- c()
        for (name in names) {
            if (is.na(name)) {
                formatted <- append(formatted, result)
                break
            }
            result <- name
            for (i in seq_along(from)) {
                if (stringr::str_detect(name, from[i])) {
                    result <- stringr::str_replace(
                        name, from[i], to[i]
                    )
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


#' @export
routine_replace <- function(df, country) {
    # filter tibble by country name
    filtered_routine_rename_database <-
        snt::import_routine_replace %>%
        dplyr::filter(country == country)
    find_and_rename <- function(names) {
        from <- filtered_routine_rename_database$from
        to <- filtered_routine_rename_database$to
        formatted <- c()
        for (name in names) {
            if (is.na(name)) {
                formatted <- append(formatted, result)
                break
            }
            result <- name
            for (i in seq_along(from)) {
                if (stringr::str_detect(name, from[i])) {
                    result <- stringr::str_replace(
                        name, from[i], to[i]
                    )
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

#' @export
update_database <- function() {
    import_routine_rename <- readr::read_csv(
        "inst//extdata//routine//01_rename.csv"
    )
    usethis::use_data(import_routine_rename)
    import_routine_replace <- readr::read_csv(
        "inst//extdata//routine//02_replace.csv"
    )
    usethis::use_data(import_routine_replace)
    import_routine_replace_hfname <- readr::read_csv(
        "inst//extdata//routine//03_replace_hfname.csv"
    )
    usethis::use_data(import_routine_replace_hfname)
    import_routine_set_cluster <- readr::read_csv(
        "inst//extdata//routine//04_cluster.csv"
    )
    usethis::use_data(import_routine_set_cluster)
    devtools::load_all()
}
