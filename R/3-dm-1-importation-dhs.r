dhs_api_endpoint <- "https://api.dhsprogram.com/rest/dhs/data/"

#' Import DHS data from API
#' @param year DHS year to be imported
#' @return A tibble of DHS data
#' @author Chunzhe ZHANG
#' @importFrom RJSONIO fromJSON
#' @importFrom data.table setDT
#' @export
#' @rdname import
import_dhs <- function(year = 2021) {
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
