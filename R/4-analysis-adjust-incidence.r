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
    mutate(n1 = conf + pres * conf / test)
}
#' Adjust incidence 2
#' @param df A data frame, calculated by adjust_incidence_1, must contain n1
#' @param reprat_df A data frame, containing the report rate of ad
#' @return Dataframe
#' @export
adjust_incidence_2 <- function(df, reprat_df) {
  df |>
    left_join(reprat_df) |>
    mutate(n2 = .data$n1 / .data$reprat)
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
      n3 = .data$n2 + .data$n2 * .data$priv_treat / .data$pub_treat +
        .data$n2 * .data$no_treat / .data$pub_treat
    )
}
#' Calculate the adjusted incidence 1
#' @param df A data frame, the routine database dataframe, with columns conf, susp, test
#' @param adm_by Columns to group by
#' @param date_by Columns to group by
#' @return Dataframe
#' @importFrom data.table := fifelse
#' @export
sn_ana_adj1 <- function(df, adm_by, date_by) {
  # consider the situation test is 0, then stop adjusting
  by_cols <- get_by_cols(adm_by, date_by)
  df[, .(conf = sn_keep_na_sum(conf),
         pres = sum(pres, na.rm = TRUE),
         test = sum(test, na.rm = TRUE)
         ),
     by = by_cols][, crude_inc := conf][, n1 := fifelse(test == 0, conf, (conf + pres * conf /
                                                                            test))]
}
#' Calculate the adjusted incidence 2
#' @param df A data table, calculated by adjust_incidence_1_dt, must contain n1
#' @param reprat_df A data frame, containing the report rate of ad
#' @param on Columns to join on
#' @importFrom data.table := fifelse
#' @export
#' @return dt
sn_ana_adj2 <- function(df, reprat_df, on) {
  df[reprat_df, on = on][, n2 := fifelse(rep_rat == 0, n1, n1 / rep_rat)]
}
#' Calculate the adjusted incidence 3
#'
#' @param df A data table, calculated by adjust_incidence_2_dt, must contain n2
#' @param treatment_seeking_df A data frame
#' @param on Columns to join on
#' @return Dataframe
#' @importFrom data.table :=
#' @export
sn_ana_adj3 <- function(df, treatment_seeking_df, on) {
  df[treatment_seeking_df, on = on][, n3 := n2 + n2 * priv_treat / pub_treat +
                                      n2 * no_treat / pub_treat]
}
#' Plot incidences
#' @param df A data frame
#' @param col Column name to plot
#' @param breaks Breaks for the color scale
#' @param palette Color palette
#' @export
#' @importFrom tmap tm_shape tm_polygons tm_facets
#' @importFrom grDevices hcl.colors
plot_map_by_year <- function(df,
                             col,
                             breaks = c(0, 250, 350, 450, 20000),
                             palette = hcl.colors(4, "Blue-Red")) {
  tm_shape(df) +
    tm_polygons(col = col,
                breaks = breaks,
                palette = palette) +
    tm_facets(by = "year")
}

bay_beta <- function(positive,
                     negative,
                     n_post_sample = 1e5,
                     target_samples = NULL,
                     plot = FALSE
                     ) {
  # If we use the Bayesian statistic,
  # The posterior distribution of Positive out of Negative is the beta distribution
  # First, sample from the posterior distribution, in this case,
  # a beta distribution
  # positive = 10
  # negative = 10
  total_test = positive + negative
  posterior_samples <-
    rbeta(n_post_sample, positive + 1, negative + 1)
  # the posterior_samples will be p from 0 - 1
  # e.g. 0.4894810 0.5015128 0.4479019 0.4622526 0.3622227
  # Second step is the use each of the samples to draw a number of
  if (is.null(target_samples))
    target_samples <- positive + negative
  posterior_distribution <- lapply(posterior_samples,
                                   function(p)
                                     stats::dbinom(0:target_samples,
                                                   target_samples,
                                                   p))
  # posterior distribution will be a matrix
  # with shape of n_post_sample * target_samples
  # Draw a sample by each of the distribution
  posterior_predictive <- lapply(posterior_distribution,
                                 function(p)
                                   sample(0:target_samples,
                                          size = 1,
                                          prob = p))

  # it will be look like [[998]]
  # [1] 11
  #
  # [[999]]
  # [1] 9
  #
  # [[1000]]
  # [1] 11
  # should be unlisted into a list
  posterior_predictive <- posterior_predictive |> unlist()
  posterior_predictive_count <- sapply(
    0: target_samples,
    function(x) sum(posterior_predictive == x)
  )
  posterior_predictive_probability <- posterior_predictive_count / n_post_sample

  result <- data.frame(
    ways = 0:target_samples,
    posterior_predictive_probability = posterior_predictive_probability)

  if(plot) {
    plot <- result |>
      ggplot(aes(x = ways, y =posterior_predictive_probability)) +
      geom_bar(stat = "identity")
    print(plot)
  }

  return(result)
}
