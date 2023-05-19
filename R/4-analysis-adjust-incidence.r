#' Adjust incidence 1
#' @param df A data frame, the routine database dataframe, with columns conf, susp, test
#' @param ... Group by columns
#' @return Dataframe
#' @import dplyr
#' @importFrom rlang enquos .data
#' @export
#' @author Chunzhe ZHANG
adjust_incidence_1 <- function(df, ...) {
  args <- enquos(...)
  df |>
    group_by(!!!args) |>
    summarise(
      conf = sum(.data$conf, na.rm = TRUE),
      susp = sum(.data$pres, na.rm = TRUE),
      test = sum(.data$test, na.rm = TRUE)
    ) |>
    mutate(n1 = conf + pres * conf/test)
}

#' Adjust incidence 2
#' @param df A data frame, calculated by adjust_incidence_1, must contain n1
#' @param reprat_df A data frame, containing the report rate of ad
#' @return Dataframe
#' @export
adjust_incidence_2 <- function(df, reprat_df) {
  df |>
    left_join(reprat_df) |>
    mutate(n2 = .data$n1/.data$reprat)
}

#' Adjust incidence 3
#' @param df A data frame, calculated by adjust_incidence_2, must contain n2
#' @param treatment_seeking_df A data frame
#' containing the treatment seeking rate
#' @return Dataframe
#' @export
adjust_incidence_3 <- function(df, treatment_seeking_df) {
  df |>
    left_join(treatment_seeking_df) |>
    mutate(
      n3 = .data$n2 + .data$n2 * .data$priv_treat/.data$pub_treat + .data$n2 *
        .data$no_treat/.data$pub_treat
    )
}

#' Calculate the adjusted incidence 1
#' @param df A data frame, the routine database dataframe, with columns conf, susp, test
#' @param adm_by Columns to group by
#' @param date_by Columns to group by
#' @return Dataframe
#' @importFrom data.table := fifelse
#' @export
adjust_incidence_1_dt <- function(df, adm_by, date_by) {
  # consider the situation test is 0, then stop adjusting
  by_cols <- get_by_cols(adm_by, date_by)
  df[, .(
    conf = sum(conf, na.rm = TRUE),
    pres = sum(pres, na.rm = TRUE),
    test = sum(test, na.rm = TRUE)
  ),
    by = by_cols][, crude_inc := conf][, n1 := fifelse(test == 0, conf, (conf + pres * conf/test))]
}

#' Calculate the adjusted incidence 2
#' @param df A data table, calculated by adjust_incidence_1_dt, must contain n1
#' @param reprat_df A data frame, containing the report rate of ad
#' @param on Columns to join on
#' @importFrom data.table := fifelse
#' @export
#' @return dt
adjust_incidence_2_dt <- function(df, reprat_df, on) {
  df[reprat_df, on = on][, n2 := fifelse(rep_rat == 0, n1, n1/rep_rat)]
}

#' Calculate the adjusted incidence 3
#'
#' @param df A data table, calculated by adjust_incidence_2_dt, must contain n2
#' @param treatment_seeking_df A data frame
#' @param on Columns to join on
#' @return Dataframe
#' @importFrom data.table :=
#' @export
adjust_incidence_3_dt <- function(df, treatment_seeking_df, on) {
  df[treatment_seeking_df, on = on][, n3 := n2 + n2 * priv_treat/pub_treat +
    n2 * no_treat/pub_treat]
}


#' Plot incidences
#' @param df A data frame
#' @param col Column name to plot
#' @param breaks Breaks for the color scale
#' @param palette Color palette
#' @export
#' @importFrom tmap tm_shape tm_polygons tm_facets
#' @importFrom grDevices hcl.colors
plot_map_by_year <- function(
  df, col, breaks = c(0, 250, 350, 450, 20000),
  palette = hcl.colors(4, "Blue-Red")
) {
  tm_shape(df) +
    tm_polygons(col = col, breaks = breaks, palette = palette) +
    tmap::tm_facets(by = "year")
}
