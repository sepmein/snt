#' @title Find outlier
#' @description Find outlier in the dataset
#' @param df dataframe
#' @param columns columns to be checked
#' @param alpha alpha value for quantile
#' @param both_sides whether to check both sides of the distribution
#' @param select_rows columns to be selected
#' @return dataframe
#' @author Chunzhe ZHANG
#' @export
#' @import dplyr
#' @importFrom tibble tibble
#' @importFrom rlang enquos
find_outliers <-
  function(df,
           alpha = 0.99999,
           both_sides = FALSE,
           ...) {

    # 1. Extract the columns we want to analyze from the dataframe.
    args <- rlang::enquos(...)
    df <- df |> gen_id_if_not_exist()

    # get numeric columns names from the dataframe
    columns <- colnames(df |> select(where(is.numeric)))
    if (length(columns) == 0) {
      stop("No numeric columns in the dataframe")
    }

    # 2. Initialize an empty tibble to store the outliers.
    result <- tibble::tibble(
      id = character(),
      value = numeric(),
      index = character()
    )

    # 3. Loop over the columns that we want to analyze.
    for (column in columns) {
# 4. Extract the upper bound for this column.
      upper_bound <- quantile(df[[column]], alpha, na.rm = TRUE)
# 5. Extract the indices of the outliers.
      outlier_index <-
        which(df[[column]] > upper_bound)
# 6. Extract the outliers as a tibble.
      df_outlier_list <- df[outlier_index, ] |>
        # select id and target column
        select("id", !!!args, {{ column }}) |>
          mutate(index = !!column) |>
          rename(value = !!column)

        # 7. Add the outliers to the result tibble.
        result <- result |> bind_rows(df_outlier_list)
    }
# 8. Add the original data to the result tibble.
    result <- result |>
      left_join(df)

      # 9. Return the result tibble.
    return(result)
  }

#' @title Plot outliers
#' @description Plot outliers
#' @param df dataframe
#' @export
plot_outliers <- function(df) {
  df |>
    select(
      year,
      allout_ov5,
      allout_u5,
      allout
    ) |>
    pivot_longer(
      cols = 2:4,
      names_to = "index",
      values_to = "value"
    ) |>
    mutate(year = factor(year)) |>
    ggplot(aes(x = value, y = index)) +
    # geom_boxplot() +
    geom_point(aes(color = year), alpha = 0.5)

  # ggsave(
  #   "SLE_outliers_1.eps",
  #   width = 5,
  #   height = 4,
  #   dpi = 300
  # )

  df |>
    select(
      year,
      maldth_ov5,
      maldth_u5,
      maldth,
      maltreat_ov5,
      maltreat_u5,
      maltreat,
      conf,
      test
    ) |>
    pivot_longer(
      cols = 2:9,
      names_to = "index",
      values_to = "value"
    ) |>
    mutate(year = factor(year)) |>
    ggplot(aes(x = value, y = index)) +
    geom_point(aes(color = year), alpha = 0.5)
  # mutate(year= factor(year)) |>
  # ggplot(aes(x = value, y = index)) +
  # geom_boxplot() +
  # # geom_point(aes(shape = year),alpha = 0.01)
  # ggstatsplot::ggbetweenstats(x = index,
  #                             y = value,
  #                             pairwise.comparisons = FALSE)
  # ggsave(
  #   "SLE_outliers_2.eps",
  #   width = 5,
  #   height = 4,
  #   dpi = 300
  # )

  df |>
    select(
      year,
      maladm,
      maladm_ov5,
      maladm_u5,
      alladm,
      alladm_ov5,
      alladm_u5,
    ) |>
    pivot_longer(
      cols = 2:7,
      names_to = "index",
      values_to = "value"
    ) |>
    # mutate(year= factor(year)) |>
    # ggplot(aes(x = value, y = index)) +
    # geom_boxplot() +
    # geom_point(aes(shape = year),alpha = 0.01)
    mutate(year = factor(year)) |>
    ggplot(aes(x = value, y = index)) +
    # geom_boxplot() +
    geom_point(aes(color = year), alpha = 0.5)
  # ggstatsplot::ggbetweenstats(x = index,
  #                             y = value,
  #                             pairwise.comparisons = FALSE)
  # ggsave(
  #   "SLE_outliers_3.eps",
  #   width = 5,
  #   height = 4,
  #   dpi = 300
  # )
}