#' Sum maltreat under 5 years old by female and male
#'
#' @param df dataframe
#' @return dataframe
#' @export
#' @examples
#' @importFrom dplyr mutate
#' sum_maltreat_under_5(df)
sum_maltreat_u5_by_sex <- function(df, maltreat_u5_f, maltreat_u5_m) {
    df |>
        mutate(
            maltreat_u5 =
                {{ maltreat_u5_f }} + {{ maltreat_u5_m }}
        )
}