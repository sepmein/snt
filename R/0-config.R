
#' @export
set_country <- function(root_folder, country) {
  #  folder <- normalizePath(file.path(root_folder, country), mustWork = FALSE)
  setwd(root_folder)
  snt_country <<- country # nolint
}

#' @export
config <- function(country,
                   root = FALSE,
                   input_root = FALSE,
                   output_root = FALSE,
                   raster_root = FALSE,
                   map_folder = FALSE,
                   ihme_folder = FALSE,
                   rainfall_folder = FALSE,
                   rainfall_output = FALSE,
                   shapefile = FALSE
                   ) {
  # TODO add a re-format function to format country to ISO3 code
  country <- country

  if (!(isFALSE(root))) {
    input_root <- file.path(root, input_root)
    output_root <- file.path(root, output_root)
    raster_root <- file.path(root, raster_root)
  }

  # output folder
  if (!(isFALSE(output_root))) {
    # set by relative path
    rainfall_output <- file.path(raster_root, rainfall_output)
    map_output <- file.path(raster_root, map_output)
    ihme_output <- file.path(raster_root, ihme_output)
  }

  # raster root folder
  if (!(isFALSE(raster_root))) {
    # Set by relative path
    rainfall_folder <- file.path(raster_root, rainfall_folder)
    map_folder <- file.path(raster_root, map_folder)
    ihme_folder <- file.path(raster_root, ihme_folder)
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
        "folder" = map_folder
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
