
cases_wmr <- snt::dbq_select(
    level = "adm1",
    adm0 = "NIGERIA",
    year_from = 2000,
    year_to = 2022,
    indicators = "cases_wmr"
)

# calculate cased averted
cases_averted <- cases_wmr |>
    dplyr::arrange(year) |>
    dplyr::select(adm1, year, cases_wmr) |>
    tidyr::pivot_wider(names_from = year, values_from = cases_wmr) |>
    dplyr::mutate(base_case = `2000`) |>
    dplyr::select(adm1, base_case, c(`2000`:`2022`)) |>
    tidyr::pivot_longer(cols = c(3:25)) |>
    dplyr::mutate(cases_averted = base_case - value) |>
    dplyr::group_by(adm1) |>
    dplyr::mutate(cases_averted_accumulated = cumsum(cases_averted)) |>
    dplyr::rename(year = name)

# plot
adm1 <- cases_averted |> dplyr::distinct(adm1)
adm1_list <- adm1$adm1

for (x in adm1_list) {
    cases_averted |> write_csv(paste0(x, " - 11. cases_averted.csv"))

    cases_averted |>
        dplyr::filter(adm1 == x) |>
        ggplot2::ggplot(ggplot2::aes(
            x = year,
            y = cases_averted_accumulated,
            group = adm1
        )) +
        ggplot2::geom_line() +
        ggplot2::geom_point() +
        ggplot2::ylab("Accumulated Estimated Cases Averted") +
        hrbrthemes::theme_ipsum() +
        ggplot2::theme(
            legend.title <- ggplot2::element_blank()
        ) +
        ggplot2::scale_x_discrete(
            guide = ggplot2::guide_axis(check.overlap = TRUE)
        )
    ggplot2::ggsave(
        paste0(x, " - 11. cases_averted.eps")
    )
}
