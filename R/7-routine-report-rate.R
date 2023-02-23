report_rate <- function(df, ...) {
    rlang::

}
## generate a list of adm0, adm1, adm2, adm3, hfname, year, month
hf_list <- routine |>
    select(adm0, adm1, adm2, adm3, hfname) |>
    unique()
year <- tibble(year = 2018:2022)
month <- tibble(month = 1:12)
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
rep <- routine |>
    mutate(
        rep = if_else(rowSums(
            across(
                allout_u5:andth
            ),
            na.rm = TRUE
        ) == 0, 0, 1)
    ) |>
    select(
        adm0, adm1, adm2, adm3, hfname,
        year, month,
        rep
    )

reprate <- exp |>
    left_join(rep) |>
    # filling NA with 0
    mutate(
        rep = if_else(is.na(rep), 0, rep)
    )

reprate |>
    group_by(adm0, adm1, adm2, adm3, hfname) |>
    summarize(
        exp = sum(exp),
        rep = sum(rep)
    ) |>
    mutate(
        reprate = rep / exp
    ) |>
    View()
