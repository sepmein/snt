#' Read a list of raster data
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
#' Read and format a series of geotiff, raster files. Rename it with `indexName`
#' The list of files should be returned by either
#' `snt::smart_get_file_list_by_year` or
#' `snt::smart_get_file_list_by_year_and_month` function.
#'
#' By default, this function use stars proxy object to speed up.
#' It is much faster than the original star read function.
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
#'
#' @param index a name for the index, for example, `pfpr`, `rainfall`, etc.
#' after aggregation, the function will return a shapefile with the aggregated
#' data, and the column name will be the index name.
#' If we don't provide the index name, the column name will be the file name
#' of the raster data. Which is not very useful.
#'
#' @export
#' @return a stars Proxy object with the attribute `indexName` and a third
#' dimension called year, which the list of year is returned by the smart read function
#' @importFrom stars read_stars st_set_dimensions
sn_read_star <- function(smart_list, index) {
  # make sure the smart_list is a list and has two elements
  # files and years
  if (!is.list(smart_list)) {
    stop("`smart_list` is not a list")
  }
  if (length(smart_list) !=
    2) {
    stop("`smart_list` does not have two elements")
  }
  if (!("files" %in% names(smart_list))) {
    stop("`smart_list` does not have a list element called `files`")
  }
  if (!("dates" %in% names(smart_list))) {
    stop("`smart_list` does not have a list element called `dates`")
  }
  # Read all the files in the list
  star <- read_stars(smart_list$files, proxy = TRUE) |>
    # Merge the files into a single stack
  merge() |>
    # Rename the bands to the year
  setNames(index) |>
    # Set the band dimension to be the year
  st_set_dimensions(
    "band", values = smart_list$year, names = "year", point = TRUE
  )
  # Return the stacked image
  return(star)
}
