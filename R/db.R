
# db connection -------
db_name <- "malaria"
db_user <- "sepmein"
db_pass <- "crimson"

# sql -----------
#' @export
sql_select_adm0_year <- "select * from adm0_year"
sql_select_adm0_month <- "select * from adm0_month"
sql_select_adm1_year <- "select * from adm1_year"
sql_select_adm1_month <- "select * from adm1_month"

## sql constrain -----------
constrain_adm0 <- function(positional_index) {
    return(
        paste0("adm0 = $", positional_index)
    )
}
constrain_adm1 <- function(positional_index) {
    return(
        paste0("adm1 = $", positional_index)
    )
}
constrain_adm2 <- function(positional_index) {
    return(
        paste0("adm2 = $", positional_index)
    )
}
constrain_year <- function(positional_index) {
    return(
        paste0("year = $", positional_index)
    )
}
constrain_range_year_included <- function(positional_index) {
    first <- positional_index
    second <- positional_index + 1
    return(
        paste0(
            "year >= $",
            first,
            " and year <= $",
            second
        )
    )
}
constrain_indicator <- function(positional_index) {
    return(
        paste0("index = $", positional_index)
    )
}
constrain_indicators <- function(positional_index, ...) {
    # index IN ('foo', 'bar')
    # index IN ($n1, $n2, $n3)
    hashes <- paste0(
        "$",
        positional_index:
        (positional_index + ... - 1)
    )
    combined_hashes <- toString(hashes)
    result <- paste0(
        "index IN (",
        combined_hashes,
        ")"
    )
    return(result)
}

## sql generator fns -------------
add_and <- function(sql) {
    sql <- paste(sql, "AND", sep = " ")
    return(sql)
}
add_where <- function(sql) {
    sql <- paste(sql, "WHERE", sep = " ")
    return(sql)
}
add_constrain <- function(sql, constain_fn, index, ...) {
    constrain <- constain_fn(index, ...)
    sql <- paste(
        sql, constrain,
        sep = " "
    )
    return(sql)
}

# query --------
## adm0_year by adm0, year, year_from, year_to, indicators
#' @export
dbq_select_adm0 <- function(adm0 = NULL,
                            year = NULL,
                            year_from = NULL,
                            year_to = NULL,
                            indicators = NULL,
                            aggregation = "yearly") {

    # Build connection
    con <- DBI::dbConnect(
        RPostgres::Postgres(),
        dbname = db_name,
        user = db_user,
        password = db_pass
    )
    if (aggregation == "yearly") {
        sql <- sql_select_adm0_year
    } else if (aggregation == "monthly") {
        sql <- sql_select_adm0_month
    }
    query_list <- c()
    index <- 1
    first <- TRUE
    modified <- FALSE

    # applied adm0 as number
    if (!is.null(adm0)) {
        if (first) {
            first <- FALSE
            sql <- add_where(sql)
        } else {
            sql <- add_and(sql)
        }
        sql <- add_constrain(
            sql, constrain_adm0, index
        )
        index <- index + 1
        query_list <- append(query_list, adm0)
        modified <- TRUE
    }
    if (!is.null(year)) {
        if (first) {
            first <- FALSE
            sql <- add_where(sql)
        } else {
            sql <- add_and(sql)
        }
        sql <- add_constrain(
            sql, constrain_year, index
        )
        index <- index + 1
        query_list <- append(query_list, year)
        modified <- TRUE
    }
    if (!is.null(indicators)) {
        length <- length(indicators)
        if (first) {
            first <- FALSE
            sql <- add_where(sql)
        } else {
            sql <- add_and(sql)
        }
        if (length == 1) {
            # example "cases_wmr"

            sql <- add_constrain(
                sql, constrain_indicator, index
            )
            index <- index + 1
            query_list <- append(query_list, indicators)
        } else if (length > 1) {
            # example c('foo', 'bar')
            sql <- add_constrain(
                sql, constrain_indicators, index, length
            )
            index <- index + length
            query_list <- append(query_list, indicators)
        }
        modified <- TRUE
    }
    if (!is.null(year_from) && !is.null(year_to)) {
        if (first) {
            first <- FALSE
            sql <- add_where(sql)
        } else {
            sql <- add_and(sql)
        }
        sql <- add_constrain(
            sql, constrain_range_year_included, index
        )
        index <- index + 2
        query_list <- append(
            query_list,
            c(year_from, year_to)
        )
        modified <- TRUE
    }
    # sending query
    dbsq_select_adm0_year <- DBI::dbSendQuery(
        con,
        sql
    )
    if (modified) {
        DBI::dbBind(dbsq_select_adm0_year, query_list)
    }
    # get result
    result <- DBI::dbFetch(dbsq_select_adm0_year)
    # clear query
    DBI::dbClearResult(dbsq_select_adm0_year)
    # close connection
    DBI::dbDisconnect(con)
    # return result
     return(tibble::as.tibble(result) |>
        tidyr::pivot_wider(
            names_from = index,
            values_from = value
        ))
}

cases_and_deaths_averted <- dbq_select_adm0(
    adm0 = "NIGERIA",
    year_from = 2010,
    year_to = 2022,
    indicators = c("cases_averted", "deaths_averted")
)
