#' @title Get odd or even rows from a tibble
#' @description Get odd or even rows from a tibble
#' @param df tibble
#' @param odd boolean, if TRUE then get odd rows, if FALSE then get even rows
#' @return tibble
#' @export
#' @import dplyr
sn_reduce_odd_or_even <- function(df, odd = TRUE) {
  subset <- seq_len(nrow(df))%%2
  if (odd) {
    result <- df[subset == 1, ]
  } else {
    result <- df[subset == 0, ]
  }
  return(result)
}
#' @title Extract month from year and week
#' @description Extract month from year and week
#'
#' This function is used to extract month from year and week. The reason why we
#' need this function is that `lubridate::week()` function returns the week
#'  number in the year, but not the month. For example, 2019-12-31 is in the
#' first week of 2020, but it is in December 2019. This function can help us to get the month. The result is a number between 1 and 12. 1 means January, 2 means February, and so on. The function is based on `lubridate::make_date()` and `lubridate::week()`.
#' The function will firstly create a date object with the first day of the
#' year, and then set the week of the date object to the week number we want.
#' After that, we can get the month of the date object.
#' @param year year in number
#' @param week week in number
#' @return month in number
#' @export
#' @importFrom lubridate make_date week month
#' @examples
#' sn_extract_month_from_year_week(2020, 1)
#' sn_extract_month_from_year_week(2020, 53)
sn_extract_month_from_year_week <- function(year, week) {
  date <- make_date(year = year, month = 1, day = 1)
  week(date) <- week
  return(month(date))
}

#' Calculate Population within 5km Radius of Health Facilities
#'
#' @param health_facilities_sf An sf object containing the locations of health facilities.
#' @param population_raster A RasterLayer or SpatRaster object with population data.
#' @param distance distance in meters, default is 5000
#' @param method method to aggregate the raster, default is sum
#' @return A data frame with health facility identifiers and the corresponding population within a 5km radius.
#' @importFrom sf st_buffer crs
#' @importFrom terra extract
sn_extract_radius <- function(
    health_facilities_sf,
    population_raster,
    area_sf,
    distance = 5000,
    method = sum) {
  # Ensure CRS compatibility
  if (st_crs(health_facilities_sf) != st_crs(area_sf)) {
    health_facilities_sf <- st_transform(health_facilities_sf, st_crs(area_sf))
  }
  # if (crs(population_raster) != st_crs(area_sf)) {
  crs_target <- st_crs(area_sf)$proj4string

  population_raster <- project(population_raster, crs_target)
  # }

  # Clip the population raster with the area shapefile
  population_raster_clipped <- terra::crop(population_raster, terra::vect(area_sf))
  population_raster_clipped <- terra::mask(population_raster_clipped, terra::vect(area_sf))

  # Buffer the health facility locations to a 5km radius, ensuring they are within the area of interest
  health_facilities_buffered <- st_buffer(health_facilities_sf, dist = distance)

  # Extract the population data for each buffered area within the clipped raster
  populations <- terra::extract(population_raster_clipped, health_facilities_buffered, fun = method, na.rm = TRUE)

  # Compile the results
  results_df <- data.frame(
    health_facility_id = health_facilities_sf$id, # Assuming there's an 'id' column in your health_facilities_sf
    population_5km_radius = populations[, 2] # Extracting the summed population values
  )

  return(results_df)
}
