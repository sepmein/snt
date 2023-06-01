dhs_api_endpoint <- "https://api.dhsprogram.com/rest/dhs/data/"

#' Import DHS data from API
#'
#' Get all DHS data from DHI.org. Given a year to be imported
#' This function will download all the data.
#' @param year DHS year to be imported
#' @return A tibble of DHS data
#' @author Chunzhe ZHANG
#' @importFrom RJSONIO fromJSON
#' @importFrom data.table setDT
#' @export
sn_get_dhs <- function(year = 2021) {
  json_file <- RJSONIO::fromJSON(paste0(dhs_api_endpoint, year, ",subnational"))
  json_data <- lapply(
    json_file$Data, function(x) {
      unlist(x)
    }
  )
  data <- as.data.frame(
    do.call("rbind", json_data),
    stringsAsFactors = FALSE
  )
  return(setDT(data))
}
