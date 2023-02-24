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
find_outlier <-
  function(df,
           columns = c(
             "susp",
             "test_rdt",
             "test_mic",
             "test_rdt_lab",
             "test",
             "abn_mic",
             "abn_rdt",
             "conf_rdt",
             "maltreat_u5",
             "maltreat_ov5",
             "maltreat",
             "maldth"
           ),
           alpha = 0.999,
           both_sides = FALSE,
           select_rows = c(
             "id",
             "adm1",
             "adm2",
             "adm3",
             "hfca",
             "hfname",
             "hf",
             "year",
             "month",
             "yearmon"
           )) {
    result <- tibble::tibble(
      id = character(),
      value = numeric(),
      index = character()
    )
    # add two columns to be added
    select_rows <- c(
      select_rows, "index",
      "value"
    )

    df <- df |> gen_id_if_not_exist()
    for (column in columns) {
      ### algorithm upper bound
      upper_bound <- quantile(df[[column]], alpha, na.rm = TRUE)
      outlier_index <-
        which(df[[column]] > upper_bound)
      df_outlier_list <- df[outlier_index, ] |>
        # select id and target column
        dplyr::select(dplyr::one_of(c("id", column))) |>
        dplyr::mutate(index = !!column) |>
        dplyr::rename(value = !!column)
      result <- result |> dplyr::full_join(df_outlier_list)
    }
    result <- result |>
      dplyr::left_join(df) |>
      dplyr::select(!!select_rows)
    return(result)
  }
