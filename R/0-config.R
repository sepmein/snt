#' Central Configuration for SNT analysis
#'
#' @param country ISO3 code for a country, e.g. nigeria should be NGA
#' @param root path of the root file, could be NULL
#' if set, will be combined with other root folders
#' @param root_input path of the input root, could be NULL
#' @param root_output path of the output root. If set, will be combined with
#'
#' @param root_raster
#' @param map_folder
#' @param ihme_folder
#' @param rainfall_folder
#' @param rainfall_output
#' @param shapefile
#'
#' @return
#' @export
#'
#' @examples
#' config(
#'   country = "NGA",
#'   root = "/Users/sepmein/snt/NGA",
#'   root_input = "input",
#'   root_output =
#'   )
#' @description Central Configuration settings for SNT analysis.
#' This function could set the following configurations:
#' 1. the target subnational analysis country
#' 2. folder structure
#'  All the folder structure could be set separately,
#' however it will be nice if they are all under one root folder.
#'  Separate setting
#'  1. root
#' 3. postgresql db connection
config <- function(country,
                   root = NULL,
                   root_input = NULL,
                   root_output = NULL,
                   root_raster = NULL,
                   raster_map_input = NULL,
                   ihme_folder = NULL,
                   rainfall_folder = NULL,
                   rainfall_output = NULL,
                   shapefile = NULL,
                   db_name = NULL,
                   db_user = NULL,
                   db_pass = NULL
                   ) {
  # TODO add a re-format function to format country to ISO3 code
  country <- country

  if (!(is.null(root))) {
    root_input <- file.path(root, root_input)
    root_output <- file.path(root, root_output)
     root_raster <- file.path(root, root_raster)
  }

  # output folder
  if (!(is.null(root_output))) {
    # set by relative path
    rainfall_output <- file.path(root_raster, rainfall_output)
    map_output <- file.path(root_raster, map_output)
    ihme_output <- file.path(root_raster, ihme_output)
  }

  # raster root folder
  if (!(is.null(root_raster))) {
    # Set by relative path
    rainfall_folder <- file.path(root_raster, rainfall_folder)
    map_folder <- file.path(root_raster, raster_map_input)
    ihme_folder <- file.path(root_raster, ihme_folder)
  }

  # cores
  cores <- parallel::detectCores()

  result <- list(
    "country" = country,
    "raster" = list(
      "rainfall" = list(
        "folder" = rainfall_folder
      ),
      "map" = list(
        "folder" = raster_map_input
      ),
      "ihme" = list(
        "folder" = ihme_folder
      )
    ),

    "parallel" = list(
      "cores" = cores
    ),
    "shapefile" = shapefile
  )
  return(result)
}

#' @title Check to use relative or absolute path
#' @description Combine root and relative check if they exist in path
#' Check relative path exists
#' if it is true then use the combined path
#' if it is false then use the relative path
#'
#' @param root String, root path
#' @param relative String, relative path
#' @return path
use_relative_or_absolute <- function(root, relative) {
  relative_path_type <- type_of_path(relative)

  if (!(isFALSE(relative_path_type))) {
    return(relative)
  }

  combined_root <- file.path(root, relative)
  combined_root_path_type <- type_of_path(combined_root)

  if (!(isFALSE(combined_root_path_type))) {
    return(combined_root)
  }

  return(FALSE)
}