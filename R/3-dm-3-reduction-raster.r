#' stars aggregate function
#'
#' Provide a stars or stars-proxy object, a shapefile and a function for
#' aggregation, this function will help you extract the raster data within
#' the shapefile, and then merge it back. A very useful function to extract GIS
#' data.
#' @param star a star object
#' @param shp a shapefile containing the area of interest to aggregate
#' the raster data. The shapefile should be in the same coordinate system
#' as the raster data. If not, this function will transform the shapefile
#' to the same coordinate system as the raster data.
#' @param fun a function for aggregation
#' @export
#' @return a shapefile with the aggregated data
#' @importFrom sf st_crs st_as_sf st_join st_transform st_equals
#' @rdname gis-reduction
sn_st_aggregate <- function(star, shp, fun) {
  # project the shapefile coordinate system with the star object
  shp <- shp |>
    st_transform(st_crs(star))
  aggregated <- aggregate(star, shp, fun, na.rm = TRUE)
  aggregated_sf <- aggregated |>
    st_as_sf(long = TRUE)
  result <- shp |>
    st_join(aggregated_sf, join = st_equals)
  # transform back
  result <- result |>
    st_transform(st_crs(shp))
  return(result)
}

#' stars crop function
#'
#' Provide a stars or stars-proxy object, a shapefile,
#' this function will help you crop the raster data within
#' the shapefile very useful function to extract GIS
#' data.
#' @param star a star object
#' @param shp a shapefile object
#' @export
#' @return a cropped stars object
#' @importFrom sf st_crs st_crop st_transform
#' @rdname gis-reduction
sn_st_crop <- function(star, shp) {
  cropped <- st_crop(
    star, shp |>
      st_transform(st_crs(star))
  )
  cropped <- cropped |>
    st_transform(st_crs(shp))
  return(cropped)
}

#' stars bbox function
#' TODO

