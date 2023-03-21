dhs_api_endpoint <- "https://api.dhsprogram.com/rest/dhs/data/"

#' Import DHS data from API
#' @param year DHS year to be imported
#' @return A tibble of DHS data
#' @author Chunzhe ZHANG
#' @importFrom RJSONIO fromJSON
#' @importFrom tibble as_tibble
#' @export
import_dhs_data <- function(year = 2021) {
  json_file <- RJSONIO::fromJSON(
    paste0(dhs_api_endpoint, year, ",subnational")
  )
  json_data <- lapply(json_file$Data, function(x) {
    unlist(x)
  })
  APIdata <- as.data.frame(
    do.call("rbind", json_data),
    stringsAsFactors = FALSE
  )
  return(tibble::as_tibble(APIdata))
}
