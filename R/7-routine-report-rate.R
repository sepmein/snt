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
