# Malaria Treatment -------
#' Sum maltreat under 5 years old by female and male
#'
#' @param df dataframe
#' @return dataframe
#' @export
#' @importFrom dplyr mutate
#' @examples
#' sum_maltreat_u5_by_sex(df)
sum_maltreat_u5_by_sex <- function(
  df, female_col = "maltreat_u5_f", male_col = "maltreat_u5_m"
) {
  df |>
    mutate(
      maltreat_u5 = {
        {
          female_col
        }
      } + {
        {
          male_col
        }
      }
    )
}
#' Sum maltreat over 5 years old by female and male
#'
#' @param df dataframe
#' @return dataframe
#' @export
#' @importFrom dplyr mutate
#' @examples
#' sum_maltreat_ov5_by_sex(df)
sum_maltreat_ov5_by_sex <- function(
  df, female_col = "maltreat_ov5_f", male_col = "maltreat_ov5_m"
) {
  df |>
    mutate(
      maltreat_ov5 = {
        {
          female_col
        }
      } + {
        {
          male_col
        }
      }
    )
}
