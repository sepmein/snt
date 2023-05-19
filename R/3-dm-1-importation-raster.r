# For extract data from raster files Written by Chunzhe ZHANG(Spencer) Email:
# sepmein@gmail.com / zhangc@who.int

#' Read a list of raster data, aggregate by shapefile
#' 
#' Read a list of raster data, aggregate by shapefile. This function is
#' designed for the raster data with a third dimension called `year`.
#' For example, we have a list of raster data, each raster data is a
#' geotiff file, and the file name is like this:
#' `pfpr_2to10_2000.tif`, `pfpr_2to10_2001.tif`, `pfpr_2to10_2002.tif`, etc.
#' The smart_read_files_by_year function will read all the files in the list
#' and return a list of raster data, each raster data is a star object, and
#' the star object has a third dimension called `year`, which is the year
#' of the raster data.
#' Internally, this function use the `stars` package to read the raster data.
#' The `stars` package is a very powerful package for reading and manipulating
#' raster data. It is much faster than the `raster` package.
#' The package use `stars` proxy object to speed up the reading process.
#' More information about the `stars` package can be found here:
#' https://r-spatial.github.io/stars/
#' 
#' @param smart_list a list of raster data
#' should be a list read from the `smart_read` functions
#' for example, we have a list of raster data, each raster data is a
#' geotiff file, and the file name is like this:
#' `pfpr_2to10_2000.tif`, `pfpr_2to10_2001.tif`, `pfpr_2to10_2002.tif`, etc.
#' The smart_read_files_by_year function will read all the files in the list
#' and return a list of raster data, each raster data is a star object, and
#' the star object has a third dimension called `year`, which is the year
#' of the raster data.
#' The smart list should be returned by the `smart_read` functions. It is
#' a list with two elements: `files` and `years`. The `files` element is a
#' list of file paths, and the `years` element is a list of years.
#' More information can be found in the `smart_read` functions.
#' @param shp a shapefile containing the area of interest to aggregate
#' the raster data. The shapefile should be in the same coordinate system
#' as the raster data. If not, this function will transform the shapefile
#' to the same coordinate system as the raster data.
#' @param fun a function to aggregate, for example, `mean`, `sum`, etc.
#' @param index a name for the index, for example, `pfpr`, `rainfall`, etc.
#' after aggregation, the function will return a shapefile with the aggregated
#' data, and the column name will be the index name.
#' If we don't provide the index name, the column name will be the file name
#' of the raster data. Which is not very useful.
#' @export
#' @return a shapefile with the aggregated data
#' @importFrom sf st_crs st_as_sf st_join st_transform st_equals
#' @importFrom stars read_stars st_set_dimensions
#' @importFrom dplyr select
#' @importFrom rlang .data
#' @rdname import
raster_read_aggregate_year <- function(smart_list, shp, fun, index) {
  # Read all the raster files in the smart_list The smart_read function has a
  # proxy argument, which is the path to the file This function will read all
  # the proxy files and combine them into a single star object
  star <- read_stars(smart_list$files, along = list(year = smart_list$years, proxy = TRUE)) |>
    setNames(index)

  # Before aggregate, we need to ensure the coordinate system of the shapefile
  # is the same as the star object. If not, we need to transform the shapefile
  # to the same coordinate system as the star object
  shp <- shp |>
    st_transform(st_crs(star))

  # Aggregate the star object by the shapefile The fun argument is a function
  # to aggregate the raster data
  aggregated <- aggregate(star, shp, fun, na.rm = TRUE)

  # Convert the aggregated star object to a sf object which will be a table
  # with columns for each year
  aggregated_sf <- aggregated |>
    st_as_sf(long = TRUE) |>
    select(-.data$proxy)

  # Join the aggregated table to the shapefile, the join method is `equals`
  # which means the aggregated table will be joined to the shapefile by the
  # `equals` method The result will be a shapefile with the aggregated data
  result <- shp |>
    st_join(aggregated_sf, join = sf::st_equals)

  return(result)
}

#' Read R stars packages from a smart list
#'
#' Read and format a series of geotiff, raster files. Rename it with `indexName`
#' The list of files should be returned by either `snt::smart_get_file_list_by_year` or
#' `snt::smart_get_file_list_by_year_and_month` function.
#'
#' By default, this function use stars proxy object to speed up. 
#' It is much faster than the original star read function.
#' @param smart_list a smart_list returned by the `snt::smart_get_file_list_by_year`
#' @export
#' @return a stars Proxy object with the attribute `indexName` and a third
#' dimension called year, which the list of year is returned by the smart read function
#' @importFrom stars read_stars st_set_dimensions
star_read <- function(smart_list, index) {
  # make sure the smart_list is a list and has two elements files and years
  if (!is.list(smart_list)) {
    stop("`smart_list` is not a list")
  }
  if (length(smart_list) !=
    2) {
    stop("`smart_list` does not have two elements")
  }
  if (!is.list(smart_list$files)) {
    stop("`smart_list` does not have a list element called `files`")
  }
  if (!is.list(smart_list$years)) {
    stop("`smart_list` does not have a list element called `years`")
  }

  # Read all the files in the list
  star <- read_stars(smart_list$files, proxy = TRUE) |>
    # Merge the files into a single stack
  merge() |>
    # Rename the bands to the year
  setNames(index) |>
    # Set the band dimension to be the year
  st_set_dimensions("band", values = smart_list$year, names = "year", point = TRUE)
  # Return the stacked image
  return(star)
}

#' stars aggregate function
#'
#' Provide a stars or stars-proxy object, a shapefile and a function for
#' aggregation, this function will help you extract the raster data within
#' the shapefile, and then merge it back. A very useful function to extract GIS
#' data.
#' @param star a star object
#' @param shp a shapefile object
#' @param fun a function for aggregation
#' @export
#' @return a shapefile with the aggregated data
#' @importFrom sf st_crs st_as_sf st_join st_transform st_equals
#' @rdname import
star_aggregate <- function(star, shp, fun) {
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
#' @rdname import
stars_crop <- function(star, shp) {
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