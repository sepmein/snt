#' Generate ADM2 shapefiles from WHO GIS Hub
#'
#' @param shp_regional_afro_adm2 Path to the shapefile of the WHO African Region
#' @param shp_regional_amro_adm2 Path to the shapefile
#' of the WHO American Region
#' @param shp_regional_emro_adm2 Path to the shapefile
#' of the WHO Eastern Mediterranean Region
#' @param shp_regional_euro_adm2 Path to the shapefile
#' of the WHO European Region
#' @param shp_regional_searo_adm2 Path to the shapefile
#' of the WHO South-East Asia Region
#' @param shp_regional_wpro_adm2 Path to the shapefile
#' of the WHO Western Pacific Region
#' @param shp_global_adm2 Path to the shapefile
#' of the WHO Global Region
#' @return A shapefile of the WHO Global Region
#' @export
#' @importFrom sf st_read st_write
#' @importFrom dplyr bind_rows
bind_who_gis_adm2_shp <- function(shp_regional_afro_adm2,
                                  shp_regional_amro_adm2,
                                  shp_regional_emro_adm2,
                                  shp_regional_euro_adm2,
                                  shp_regional_searo_adm2,
                                  shp_regional_wpro_adm2,
                                  shp_global_adm2) {
  shp_regional_afro_adm2 <- st_read(shp_regional_afro_adm2)
  shp_regional_amro_adm2 <- st_read(shp_regional_amro_adm2)
  shp_regional_emro_adm2 <- st_read(shp_regional_emro_adm2)
  shp_regional_euro_adm2 <- st_read(shp_regional_euro_adm2)
  shp_regional_searo_adm2 <- st_read(shp_regional_searo_adm2)
  shp_regional_wpro_adm2 <- st_read(shp_regional_wpro_adm2)

  shp_global_adm2 <- bind_rows(
    shp_regional_afro_adm2, shp_regional_amro_adm2
  ) |>
    bind_rows(shp_regional_emro_adm2) |>
    bind_rows(shp_regional_emro_adm2) |>
    bind_rows(shp_regional_euro_adm2) |>
    bind_rows(shp_regional_searo_adm2) |>
    bind_rows(shp_regional_wpro_adm2)

  shp_global_adm2 |> st_write(
    shp_global_adm2,
    layer_options = "ENCODING=UTF-8"
  )
  return(shp_global_adm2)
}

#' extract a country from WHO GIS hub adm2 shapefile
#'
#' @param shp_global_adm2 Path to the shapefile of the WHO Global Region
#' @param iso3 ISO3 code Country to extract
#' @param save_path Path to save the shapefile
#' @export
#' @importFrom sf st_read st_write
#' @importFrom dplyr filter
#' @importFrom rlang .data
extract_country_from_who_gis_adm2_shp <- function(shp_global_adm2,
                                                  iso3,
                                                  save_path) {
  # read the shapefile
  shp_global_adm2 <- st_read(shp_global_adm2)

  # filter the shapefile by ISO3 code
  shp_country <- shp_global_adm2 |>
    filter(.data$ISO_3_CODE == iso3)

  # check if the filtered shapefile is empty
  if (nrow(shp_country) == 0) {
    stop("No shapefile for the given ISO3 code. Please check the code.")
  }

  # write the filtered shapefile
  shp_country |> st_write(
    save_path,
    layer_options = "ENCODING=UTF-8"
  )
}
