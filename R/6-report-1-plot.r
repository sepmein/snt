#' Generate a rainfall plot
#'
#' A function to generate a rainfall boxplot
#' Should contain at least the following columns: adm1, year, month, rainfall
#' @param rainfall a tibble or a data.table
#' @export
#' @importFrom ggplot2 aes geom_boxplot labs theme
#' @importFrom cowplot theme_cowplot
#' @importFrom wesanderson wes_palette
gen_plot_rainfall <- function(rainfall) {
  result <- rainfall |>
    mutate(month = factor(month, labels = month.abb)) |>
    ggplot(aes(month, rainfall, group = month)) +
    geom_boxplot(fill = wesanderson::wes_palette("Darjeeling1")[3]) +
    labs(x = "Month", y = "Rainfall (mm)") +
    cowplot::theme_cowplot() + theme(axis.text = element_text(size = 6.6))
}
