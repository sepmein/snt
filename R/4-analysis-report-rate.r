#' Plot Report Status
#' @param df A data frame
#' @return Dataframe
#' @import dplyr
#' @importFrom rlang enquos .data
#' @import ggplot2
#' @importFrom lubridate make_date
#' @export
plot_hf_report_status <- function(report_status) {
  # TODO how to solve the problem of month is not included
  # in args TODO how to solve the problem of hf not
  # included in args TODO solve the preset argument
  # problem, hfname, reported, year, month
  # make robust hfname, year and month are required check
  # if hfname is present
  stopifnot("hf" %in% names(report_status))
  # check if year is present
  stopifnot("year" %in% names(report_status))
  # check if month is present
  stopifnot("month" %in% names(report_status))
  report_status[, `:=`(
    date = make_date(year, month, 1),
    rep = fifelse(rep == 0, "N", "Y")
  )] |>
    ggplot(aes(x = date, y = hf, fill = rep)) +
    geom_tile() + scale_x_date(date_labels = "%Y", guide = guide_axis(check.overlap = T)) +
    labs(
      title = "Report Status", y = "Health Facilities",
      x = "Date"
    ) +
    snt::theme_snt() + theme(axis.text.y = element_blank()) +
    scale_fill_manual(values = c(N = "white", Y = "blue"))
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
plot_indicator_report_rate <- function(rep_rat, indicator) {
  rep_rat[, ..indicator] |>
    ggplot() + geom_tile(aes(x = .data$year, y = .data$index, fill = .data$rep_rat)) +
    scale_fill_viridis_c() + theme_snt()
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
plot_adm1_report_rate <- function(rep_rat) {
  # todo change this to more flexible argument
  rep_rat[, date := make_date(year, month, day = 1)]
  ggplot(rep_rat) +
    geom_tile(aes(x = .data$date, y = .data$adm1, fill = .data$rep_rate)) +
    scale_fill_viridis_c() + scale_x_discrete(guide = guide_axis(check.overlap = TRUE)) +
    scale_x_date(date_labels = "%Y-%m") +
    labs(
      title = "Report Status By Adm1 and Date", y = "Districts",
      x = "Date"
    ) +
    theme_snt()
}
