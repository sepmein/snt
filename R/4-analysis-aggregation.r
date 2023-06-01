#' @title Aggregate_by, a helper function for generating the aggregation
#' information
#' @description Aggregate data by admin level and date level
#' @param admin_level Admin level, could be one of 'adm1', 'adm2', 'hf'
#' @param date_level Date level could be one of 'year', 'month'
#' @export
#' @examples
#' group_by('adm1', 'year')
#' group_by('adm1', 'month')
#' group_by('adm2', 'year')
#' group_by('adm2', 'month')
#' group_by('hf', 'year')
#' group_by('hf', 'month')
sn_aggregate_by <- function(admin_level, date_level) {
  # choose from admin_level, if adm1, then produce, adm1,
  # if adm2, then produce adm1, adm2, if hf, then produce
  # adm1, adm2, hf choose from date_level, if year, then
  # produce year, if month, then produce year, month
  if (admin_level == "adm1") {
    if (date_level == "year") {
      return(c("adm1", "year"))
    } else if (date_level == "month") {
      return(c("adm1", "year", "month"))
    }
  } else if (admin_level == "adm2") {
    if (date_level == "year") {
      return(c("adm1", "adm2", "year"))
    } else if (date_level == "month") {
      return(c("adm1", "adm2", "year", "month"))
    }
  } else if (admin_level == "hf") {
    if (date_level == "year") {
      return(c("adm1", "adm2", "hf", "year"))
    } else if (date_level == "month") {
      return(c("adm1", "adm2", "hf", "year", "month"))
    }
  } else {
    stop(
      "admin_level must be one of adm1, adm2, hf, date_level must be one of year, month"
    )
  }
}
