#' Plot Report Status
#' @param df A data frame
#' @param ... Columns to group by
#' @return Dataframe
#' @import dplyr
#' @importFrom rlang enquos
#' @import ggplot2
#' @importFrom lubridate make_date
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