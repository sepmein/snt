#' @title Fuzzy match
#' @description Fuzzy match function, check this answer for more details:
#' https://stackoverflow.com/a/41560524/886198
#'
#' @param df Dataframe
#' @param col Column to match
#' @param method Method for fuzzy match
#' @param threshold Threshold for fuzzy match
#' @return Dataframe
#' @export
#' @examples
#' df <- data.frame(
#'     col = c("a", "b", "c", "aa")
#' )
#' fuzzy_match(df, col, match_col)
#' fuzzy_match(df, col, match_col, threshold = 1)
#' @author [email protected]
#' @importFrom stringdist stringdistmatrix
#' @importFrom tibble as_tibble
#' @importFrom dplyr filter
fuzzy_match <- function(df, col, threshold = 1.1,
                        method = "lv", ...) {
    match <- c(NA)
    result_df <- data.frame(df[[col]], match, stringsAsFactors = FALSE)
    colnames(result_df) <- c(col, "match")
    match_matrix <- stringdistmatrix(
        df[[col]], df[[col]],
        method = method
    )
    match_matrix[match_matrix == 0] <- NA # ignore self
    match_matrix[match_matrix > threshold] <- NA # cut level
    amatch <- rowSums(match_matrix, na.rm = TRUE) > 0 # ignore no match
    result_df[["match"]][amatch] <- result_df[[col]][apply(
        match_matrix[amatch, ], 1, which.min
    )]
    return(as_tibble(result_df) |> filter(!is.na(match)))
}