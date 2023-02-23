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
    hf_list <- df |>
        select(!!!args) |>
        unique()
    min_year <- min(df$year)
    max_year <- max(df$year)
    year <- tibble(year = min_year:max_year)
    month <- tibble(month = 1:12)
    ## generate a list of adm0, adm1, adm2, adm3, hfname, year, month
    hf_list_ym <- hf_list |>
        cross_join(year) |>
        cross_join(month)
    exp <- hf_list_ym |>
        mutate(
            exp = 1
        )
    ## for each adm0, adm1, adm2, adm3, hfname, year, month
    ## calculate the reporting rate for each indicator
    ## summing over all, if it is zero,
    ## then consider it as not reported, else reported
    rep <- df |>
        mutate(
            rep = if_else(rowSums(
                across(
                    !c(!!!args, year, month)
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
