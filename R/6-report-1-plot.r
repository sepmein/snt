#' Generate a rainfall plot
#'
#' A function to generate a rainfall boxplot
#' Should contain at least the following columns: adm1, year, month, rainfall
#' @param rainfall a tibble or a data.table
#' @export
#' @importFrom ggplot2 aes geom_boxplot labs theme
#' @importFrom cowplot theme_cowplot
#' @importFrom wesanderson wes_palette
sn_plot_rainfall <- function(rainfall) {
  result <- rainfall |>
    mutate(month = factor(month, labels = month.abb)) |>
    ggplot(aes(month, rainfall, group = month)) +
    geom_boxplot(fill = wesanderson::wes_palette("Darjeeling1")[3]) +
    labs(x = "Month", y = "Rainfall (mm)") +
    cowplot::theme_cowplot() + theme(axis.text = element_text(size = 6.6))
}
#' @title Plot outliers by year
#' @description Generate a plot for outliers, with year as color
#' and index as x-axis, value as y-axis
#' @param d data.table
#' @param save_to path to save the plot
#' @param ... columns to be plotted, should be character vector
#' for example, 'a', 'b', 'c'
#' @return plot
#' @author Chunzhe ZHANG
#' @importFrom data.table melt
#' @importFrom ggplot2 ggplot aes geom_point ggsave
#' @export
sn_plot_outliers <- function(d, ..., width = 6, height = 4, alpha = 0.4, save_to = NULL) {
  args <- list(...) |>
    unlist()
  melted <- melt(
    d, id.vars = "year", measure.vars = args, variable.name = "index",
    value.name = "value"
  )
  melted <- melted[!is.na(value)]
  melted[, index := factor(index)]
  melted[, year := factor(year)]
  melted[, value := as.numeric(value)]
  plot <- melted |>
    ggplot(aes(x = .data$value, y = .data$index)) +
    # geom_boxplot() +
  geom_point(
    aes(color = .data$year),
    alpha = alpha
  ) +
    sn_theme()
  if (!is.null(save_to)) {
    ggsave(save_to, plot, width = width, height = height, dpi = 300)
  }
  return(plot)
}
#' Plot incidences
#' @param df A data frame
#' @param col Column name to plot
#' @param breaks Breaks for the color scale
#' @param palette Color palette
#' @export
#' @importFrom tmap tm_shape tm_polygons tm_facets
#' @importFrom grDevices hcl.colors
sn_plot_incidence_by_year <- function(
  df, col, breaks = c(0, 250, 350, 450, 20000),
  palette = hcl.colors(4, "Blue-Red")
) {
  tm_shape(df) +
    tm_polygons(col = col, breaks = breaks, palette = palette) +
    tm_facets(by = "year")
}
#' @title Consistency check plot
#' @description Consistency check
#' @param df dataframe
#' @param index_a smaller index for comparison
#' @param index_b larger index for comparison
#' @return plot
#' @author Chunzhe ZHANG
#' @export
#' @import dplyr
#' @import ggplot2
sn_plot_consistency <- function(d, var_small, var_big) {
  d <- d[!is.na(base::get(var_small)) &
    !is.na(base::get(var_big))]
  d[, comparison := fifelse(
    base::get(var_small) <=
      base::get(var_big),
    TRUE, FALSE
  )]
  d |>
    ggplot() + geom_point(
    aes(
      y = base::get(var_big),
      x = base::get(var_small),
      color = .data$comparison
    )
  ) +
    xlab(var_small) +
    ylab(var_big) +
    scale_color_manual(
      values = c(`TRUE` = sn_color_true, `FALSE` = sn_color_false)
    ) +
    geom_abline(slope = 1) +
    sn_theme()
}
#' Plot report rate by index
#'
#' This function plots the report rate by index and year
#' for each index, the report rate is calculated as the number of
#' reports divided by the number of cases for that index. This package
#' has also provided a function to calculate the report rate, which is called
#' \code{sn_ana_reprat}.
#' @param d data.table or data.frame, should contain at least the following
#' columns: year, month, variable, value, rep_rat
#' @param save_to path to save the plot
#' @return plot
#' @author Chunzhe ZHANG
#' @importFrom lubridate make_date
#' @importFrom ggplot2 ggplot aes geom_tile scale_x_date scale_fill_viridis_c
#' @export
sn_plot_reprat_by_index <- function(d, save_to) {
  # test if all required columns are present
  if (!all(
    c("year", "month", "variable", "rep_rat") %in%
      names(d)
  )) {
    stop(
      "The data should contain at least the following columns: year, month, variable, rep_rat"
    )
  }
  # year and month should be numeric
  d[, year := as.numeric(year)]
  d[, month := as.numeric(month)]
  # create a date column
  d[, date := make_date(year, month, 1)]
  # create a plot
  plot <- d |>
    ggplot() + geom_tile(
    aes(x = .data$date, y = .data$variable, fill = .data$rep_rat)
  ) +
    scale_x_date(
      date_labels = "%Y", guide = guide_axis(check.overlap = TRUE),
      date_breaks = "1 year"
    ) +
    scale_fill_viridis_c() + sn_theme()
  ggsave(save_to, plot, width = 16, height = 9, dpi = 300)
  return(plot)
}
#' Plot report rate by adm1
#'
#' This function plots the similar plot as \code{sn_plot_reprat_by_index},
#' but the y-axis is adm1 instead of index.
#'
#' For each adm1, the report rate is calculated as the number of
#' reports divided by the number of cases for that adm1. This package
#' has also provided a function to calculate the report rate, which is called
#' \code{sn_ana_reprat}. Please use it to calculate the report rate before
#' plotting.
#' @param d data.table or data.frame, should contain at least the following
#' columns: year, month, adm1, value, rep_rat
#' @param save_to path to save the plot
#' @return plot
#' @author Chunzhe ZHANG
#' @importFrom lubridate make_date
#' @importFrom ggplot2 ggplot aes geom_tile scale_x_date scale_fill_viridis_c
#' @export
sn_plot_reprat_by_adm <- function(d, save_to) {
  d[, date := make_date(year, month, 1)]
  plot <- d[!is.na(adm1)] |>
    ggplot() + geom_tile(aes(x = .data$date, y = .data$adm1, fill = .data$rep_rat)) +
    scale_x_date(
      date_labels = "%Y", guide = guide_axis(check.overlap = TRUE),
      date_breaks = "1 year"
    ) +
    scale_fill_viridis_c() + sn_theme()
  ggsave(save_to, plot, width = 16, height = 9, dpi = 300)
  return(plot)
}
