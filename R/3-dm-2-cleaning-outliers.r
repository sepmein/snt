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
#' @importFrom rlang enquos .data
sn_check_outliers <- function(df, alpha = 0.99999, both_sides = FALSE, ...) {
  # 1. Extract the columns we want to analyze from the dataframe.
  args <- rlang::enquos(...)
  df <- df |>
    gen_id_if_not_exist()
  # get numeric columns names from the dataframe
  columns <- colnames(
    df |>
      select(where(is.numeric))
  )
  if (length(columns) ==
    0) {
    stop("No numeric columns in the dataframe")
  }

  # 2. Initialize an empty tibble to store the outliers.
  result <- tibble::tibble(id = character(), value = numeric(), index = character())

  # 3. Loop over the columns that we want to analyze.
  for (column in columns) {
    # 4. Extract the upper bound for this column.
    upper_bound <- quantile(df[[column]], alpha, na.rm = TRUE)
    # 5. Extract the indices of the outliers.
    outlier_index <- which(df[[column]] > upper_bound)
    # 6. Extract the outliers as a tibble.
    df_outlier_list <- df[outlier_index, ] |>
      # select id and target column
    select(
      "id", !!!args, {
        {
          column
        }
      }
    ) |>
      mutate(index = !!column) |>
      rename(value = !!column) |>
      # to work with data.table
    mutate(id = as.character(.data$id)) |>
      mutate(index = as.character(.data$index)) |>
      mutate(value = as.numeric(.data$value))

    # 7. Add the outliers to the result tibble.
    result <- result |>
      bind_rows(df_outlier_list)
  }
  # 8. Add the original data to the result tibble.
  result <- result |>
    left_join(df) |>
    select(id, !!!args, .data$index, .data$value)

  # 9. Return the result tibble.
  return(result)
}

#' Data.table version of find_outliers
#'
sn_clean_outliers <- function(dt) {
  # Create an empty list to store the outliers removed for each column
  outliers_removed <- list()

  # Loop through each column in the data.table
  for (col in names(dt)) {
    # Calculate the 99th percentile for the column
    q99 <- quantile(dt[[col]], 0.99)

    # Identify the outliers in the column
    outliers <- dt[[col]][dt[[col]] > q99]

    # Remove the outliers from the column
    dt[[col]] <- dt[[col]][dt[[col]] <= q99]

    # Add the outliers to the list of outliers removed
    outliers_removed[[col]] <- outliers
  }

  # Return the data.table with outliers removed and the list of outliers
  # removed
  return(list(dt = dt, outliers_removed = outliers_removed))
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
plot_outliers <- function(df, ..., save_to = NULL) {
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
