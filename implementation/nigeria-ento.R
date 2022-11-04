library(tidyverse)
library(readxl)
library(ggplot2)
library(hrbrthemes)
library(haven)

# ento -----
ento_path <- "~/Desktop/IR_DB_20220830.xlsx"
ento <- read_xlsx(ento_path, sheet = "Discriminating")

ento <- ento |>
    filter(WHO_COUNTRY_NAME == "Nigeria")

ento_result <- ento |>
    group_by(WHO_YEAR_START, WHO_TEST_INSECTICIDE_CLASS) |>
    count(WHO_TEST_RESISTANCE_STATUS) |>
    pivot_wider(names_from = WHO_TEST_RESISTANCE_STATUS, values_from = n) |>
    mutate(total = sum(
        `Confirmed resistance`, `Possible resistance`, `Susceptible`, `Undetermined`,
        na.rm = TRUE
    )) |>
    mutate(
        `% Confirmed resistance` = `Confirmed resistance` / total,
        `% Possible resistance` = `Possible resistance` / total,
        `% Susceptible` = `Susceptible` / total,
        `% Undetermined` = `Undetermined` / total
    ) |>
    rename(
        year = WHO_YEAR_START,
        insecticide_class = WHO_TEST_INSECTICIDE_CLASS
    ) |>
    select(
        year,
        insecticide_class,
        `% Confirmed resistance`,
        `% Possible resistance`,
        `% Susceptible`,
        `% Undetermined`,
    ) |>
    filter(year > 2013)

View(ento_result)

ento_result |>
    pivot_longer(c(2:5)) |>
    ggplot(aes(fill = name, y = value, x = year)) +
    geom_bar(
        position = "stack", stat = "identity"
    ) +
    ylab("% resistance") +
    theme(legend.title = element_blank()) +
    facet_wrap(~insecticide_class)

ggsave("nigeria_ento_classes.png")

# cases_averted

cases_and_deaths_averted <- dbq_select_adm0(
    adm0 = "NIGERIA",
    year_from = 2010,
    year_to = 2022,
    indicators = c("cases_averted", "deaths_averted")
)
View(cases_and_deaths_averted)

scale <- mean(cases_and_deaths_averted$`cases_averted       `) / mean(cases_and_deaths_averted$`deaths_averted      `)

cases_and_deaths_averted |>
    pivot_longer(cols = c(3:4)) |>
    ggplot(aes(x = year, y = value, group = name, color = name, )) +
    geom_line(aes(linetype = name)) +
    geom_point(aes(shape = name)) +
    facet_wrap(~name, scales = "free_y", nrow = 2) +
    theme_ipsum() +
    theme(
        legend.position = "none",
        axis.title.y = element_blank()
    )

ggsave("nigeria_cases_mortality_averted.png")

p_hf_routine <- "/Users/sepmein/Library/CloudStorage/OneDrive-WorldHealthOrganization/NGA/WHO_NGA/NGA_2022_SNT/_Submitted_data/Routine\ data/hflevel_14-20.dta"

hf_routine <- read_dta(p_hf_routine)
ipt <- hf_routine |>
    select(adm1, year, month, anc_total, ipt1, ipt2) |>
    mutate(
        month_pad = stringr::str_pad(month, 2, side = "left", pad = "0")
    ) |>
    mutate(date = paste0(year, "-", month_pad, "-01")) |>
    mutate(date = as.Date(date)) |>
    select(-year, -month, -month_pad) |>
    group_by(adm1, date) |>
    summarise_all(~ sum(., na.rm = TRUE)) |>
    mutate(ipt1_cov = ipt1 / anc_total, ipt2_cov = ipt2 / anc_total) |>
    select(adm1, date, ipt1_cov, ipt2_cov) |>
    pivot_longer(
        cols = c(3:4)
    )

adm1s <- hf_routine |>
    select(adm1) |>
    unique()
for (x in adm1s$adm1) {
    ipt |>
        filter(adm1 == x) |>
        filter(value <= 1) |>
        ggplot(aes(x = date, y = value, group = name, color = name)) +
        geom_line(aes(linetype = name)) +
        geom_point(aes(shape = name)) +
        ylab("%") +
        theme_ipsum() +
        theme(
            legend.title = element_blank()
        )
    ggsave(
        paste0(x, " ipt-cov.png")
    )
}
