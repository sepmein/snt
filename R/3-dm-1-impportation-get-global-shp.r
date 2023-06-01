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
sn_get_who_gis_adm2_shp <- function(
    shp_regional_afro_adm2, shp_regional_amro_adm2,
    shp_regional_emro_adm2, shp_regional_euro_adm2,
    shp_regional_searo_adm2, shp_regional_wpro_adm2,
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

  shp_global_adm2 |>
    st_write(shp_global_adm2, layer_options = "ENCODING=UTF-8")
  return(shp_global_adm2)
}
