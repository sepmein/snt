#' Calculate the reporting rate for each indicator
#' @param df A data frame
#' @param ... Columns to calculate the reporting rate, excluding year and month
#' @return Dataframe
#' @import dplyr
#' @importFrom rlang enquos
#' @importFrom tibble tibble
#' @export
report_rate <- function(df, ...) {
    args <- enquos(...)
    target_list <- df |>
        select(!!!args) |>
        unique()
    min_year <- min(df$year)
    max_year <- max(df$year)
    year <- tibble(year = min_year:max_year)
    month <- tibble(month = 1:12)
    target_list_ym <- target_list |>
        cross_join(year) |>
        cross_join(month)
    exp <- target_list_ym |>
        mutate(
            exp = 1
        )
    rep <- df |>
        mutate(
            rep = if_else(rowSums(
                across(
                    !c(!!!args, year, month) & where(is.numeric)
                ),
                na.rm = TRUE
            ) == 0, 0, 1)
        ) |>
        select(
            !!!args, year, month,
            rep
        )

    reprate <- exp |>
        left_join(rep) |>
        # filling NA with 0
        mutate(
            rep = if_else(is.na(rep), 0, rep)
        )

    reprate |>
        group_by(!!!args) |>
        summarize(
            exp = sum(exp),
            rep = sum(rep)
        ) |>
        mutate(
            reprate = rep / exp
        )
}

#' Calculate the report status
#'
#' If any indicator is reported, the report status is "Y", otherwise "N"
#' grouped by the provided columns
#' @param df A data frame
#' @param ... Columns to group by
#' @return Dataframe
#' @import dplyr
#' @importFrom rlang enquos
#' @export
report_status <- function(df, ...) {
    # create a list of arguments
    args <- enquos(...)
    # group data frame by arguments
    df <- df |>
        group_by(!!!args) |>
        # calculate sum for each group for all numeric columns
        summarise(
            across(
                !any_of(c("year", "month")) &
                    where(is.numeric),
                ~ sum(.x, na.rm = TRUE)
            )
        )
    # test year or month is included in args or not
    if (!("year" %in% args)) {
        # create reported column
        df <- df |>
            mutate(
                reported = if_else(rowSums(
                    across(
                        !any_of(c("year", "month")) & where(is.numeric)
                    ),
                    na.rm = TRUE
                ) == 0, 0, 1)
            )
    } else {
        if (!("month" %in% args)) {
            # create reported column
            df <- df |>
                mutate(
                    reported = if_else(rowSums(
                        across(
                            !c("month") & where(is.numeric)
                        ),
                        na.rm = TRUE
                    ) == 0, 0, 1)
                )
        } else {
            # create reported column
            df <- df |>
                mutate(
                    reported = if_else(rowSums(
                        across(
                            where(is.numeric)
                        ),
                        na.rm = TRUE
                    ) == 0, 0, 1)
                )
        }
    }
    # select only arguments and reported column
    df |>
        select(
            !!!args, "reported"
        ) |>
        # convert reported to character
        mutate(
            reported = if_else(reported > 0, "Y", "N")
        )
}