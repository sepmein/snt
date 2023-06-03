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
#' @title Plot outliers
#' @description Plot outliers
#' @param df dataframe
#' @param save_to path to save the plot
#' @param ... columns to be plotted
#' @return plot
#' @author Chunzhe ZHANG
#' @import dplyr
#' @importFrom rlang enquos
#' @importFrom ggplot2 ggplot aes geom_point ggsave
#' @export
sn_plot_outliers <- function(df, ..., save_to = NULL) {
  args <- rlang::enquos(...)
  plot <- df |>
    select(.data$year, !!!args) |>
    pivot_longer(
      cols = c(!!!args),
      names_to = "index", values_to = "value"
    ) |>
    mutate(year = factor(year)) |>
    ggplot(aes(x = .data$value, y = .data$index)) +
    # geom_boxplot() +
  geom_point(
    aes(color = .data$year),
    alpha = 0.5
  )
  if (!is.null(save_to)) {
    plot |>
      ggsave(save_to, width = 5, height = 4, dpi = 300)
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
sn_plot_consistency <- function(df, index_a, index_b) {
  df |>
    mutate(
      comparison = if_else(
        {
          {
          index_a
          }
        } < {
          {
          index_b
          }
        }, TRUE, FALSE
      )
    ) |>
    ggplot() + geom_point(
    aes(
      y = {
        {
          index_a
        }
      }, x = {
        {
          index_b
        }
      }, color = .data$comparison
    )
  ) +
    geom_abline(slope = 1) +
    sn_theme()
}

