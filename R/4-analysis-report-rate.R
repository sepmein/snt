#' Plot Report Status
#' @param df A data frame
#' @param ... Columns to group by
#' @return Dataframe
#' @import dplyr
#' @importFrom rlang enquos
#' @import ggplot2
#' @importFrom lubridate make_date
#' @export
plot_hf_report_status <- function(df, ...) {
    # TODO how to solve the problem of month is not included in args
    # TODO how to solve the problem of hf not included in args
    # TODO solve the preset argument problem, hfname, reported, year, month

    args <- enquos(...)
    # select numeric columns excerpt for year and month
    report_status <- df |> report_status(!!!args)

    # make robust
    # hfname, year and month are required
    # check if hfname is present
    stopifnot("hfname" %in% names(report_status))
    # check if year is present
    stopifnot("year" %in% names(report_status))
    # check if month is present
    stopifnot("month" %in% names(report_status))

    report_status |>
        mutate(
            date = make_date(year, month, 1)
        ) |>
        ggplot(aes(x = date, y = hfname, fill = reported)) +
        geom_tile() +
        scale_x_discrete(guide = guide_axis(check.overlap = TRUE)) +
        scale_x_date(date_labels = "%Y-%m") +
        labs(
            title = "Report Status",
            y = "Health Facilities",
            x = "Date"
        ) +
        theme(axis.text.y = element_blank()) +
        scale_fill_manual(values = c("N" = "white", "Y" = "blue"))
}

#' Plot Report Status by indicators
#' @param df A data frame
#' @param ... indicator columns
#' @return Dataframe
#' @import dplyr
#' @importFrom rlang enquos .data
#' @import ggplot2
#' @importFrom lubridate make_date
#' @importFrom tidyr pivot_longer
#' @export
#' @author Chunzhe ZHANG
plot_indicator_report_status <- function(df, ...) {
    args <- enquos(...)

    report_status <- df |>
        pivot_longer(
            cols = c(!!!args),
            names_to = "index",
            values_to = "value"
        ) |>
        mutate(reported = if_else(.data$value > 0, 1, 0)) |>
        group_by(.data$index, .data$year) |>
        summarise(
            reports = sum(.data$reported, na.rm = TRUE),
            n = n()
        ) |>
        mutate(report_rate = .data$reports / n)

    report_status |> View()
    ggplot(report_status) +
        geom_tile(aes(
            x = .data$year,
            y = .data$index,
            fill = .data$report_rate
        )) +
        scale_fill_viridis_c()
}

#' Plot Report Status by district month
#' @param df A data frame
#' @param ... Group by columns
#' @param index Index columns
#' @return Dataframe
#' @import dplyr
#' @importFrom rlang enquos .data
#' @import ggplot2
#' @importFrom lubridate make_date
#' @importFrom tidyr pivot_longer
#' @export
plot_adm_report_status <- function(
    df, ...,
    index = c("conf", "test", "susp")) {
    args <- enquos(...)

    report_status <- df |>
        pivot_longer(
            cols = index,
            names_to = "index",
            values_to = "value"
        ) |>
        mutate(reported = if_else(.data$value > 0, 1, 0)) |>
        group_by(!!!args, .data$year, .data$month) |>
        summarise(reports = sum(.data$reported, na.rm = TRUE), n = n()) |>
        mutate(report_rate = .data$reports / n) |>
        mutate(date = make_date(.data$year, .data$month, 1))

    ggplot(report_status) +
        geom_tile(aes(
            x = .data$date,
            # todo change this to more flexible argument
            y = .data$adm1,
            fill = .data$report_rate
        )) +
        scale_fill_viridis_c() +
        scale_x_discrete(guide = guide_axis(check.overlap = TRUE)) +
        scale_x_date(date_labels = "%Y-%m") +
        labs(
            title = "Report Status By Adm1 and Date",
            y = "Districts",
            x = "Date"
        )
}