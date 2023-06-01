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
#' @rdname gis
bind_who_gis_adm2_shp <- function(
  shp_regional_afro_adm2, shp_regional_amro_adm2, shp_regional_emro_adm2, shp_regional_euro_adm2,
  shp_regional_searo_adm2, shp_regional_wpro_adm2, shp_global_adm2
) {
  shp_regional_afro_adm2 <- st_read(shp_regional_afro_adm2)
  shp_regional_amro_adm2 <- st_read(shp_regional_amro_adm2)
  shp_regional_emro_adm2 <- st_read(shp_regional_emro_adm2)
  shp_regional_euro_adm2 <- st_read(shp_regional_euro_adm2)
  shp_regional_searo_adm2 <- st_read(shp_regional_searo_adm2)
  shp_regional_wpro_adm2 <- st_read(shp_regional_wpro_adm2)

  shp_global_adm2 <- bind_rows(shp_regional_afro_adm2, shp_regional_amro_adm2) |>
    bind_rows(shp_regional_emro_adm2) |>
    bind_rows(shp_regional_emro_adm2) |>
    bind_rows(shp_regional_euro_adm2) |>
    bind_rows(shp_regional_searo_adm2) |>
    bind_rows(shp_regional_wpro_adm2)

  shp_global_adm2 |>
    st_write(shp_global_adm2, layer_options = "ENCODING=UTF-8")
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
#' @rdname gis
extract_country_from_who_gis_adm2_shp <- function(shp_global_adm2, iso3, save_path) {
  # read the shapefile
  shp_global_adm2 <- st_read(shp_global_adm2)

  # filter the shapefile by ISO3 code
  shp_country <- shp_global_adm2 |>
    filter(.data$ISO_3_CODE == iso3)

  # check if the filtered shapefile is empty
  if (nrow(shp_country) ==
    0) {
    stop("No shapefile for the given ISO3 code. Please check the code.")
  }

  # write the filtered shapefile
  shp_country |>
    st_write(save_path, layer_options = "ENCODING=UTF-8")
}

#' extract adm1 adm2 list from WHO GIS hub adm2 shapefile
#'
#' @param shp_global_adm2 Path to the shapefile of the WHO Global Region
#' @return A list of adm1 adm2 with GUID
#' @export
#' @importFrom sf st_read st_drop_geometry
#' @importFrom dplyr filter distinct select arrange
#' @importFrom rlang .data
#' @rdname gis
extract_adm1_adm2_from_who_gis_adm2_shp <- function(shp_global_adm2) {
  # read the shapefile
  shp_global_adm2 <- st_read(shp_global_adm2) |>
    st_drop_geometry()

  # extract adm1 and adm2 list
  adm1_adm2_list <- shp_global_adm2 |>
    select(
      .data$ADM0_NAME, .data$ADM0_GUID, .data$ISO_3_CODE, .data$ADM1_NAME,
      .data$ADM2_NAME, .data$ADM1_GUID, .data$GUID
    ) |>
    rename(ADM2_GUID = .data$GUID) |>
    distinct() |>
    arrange(.data$ADM1_NAME, .data$ADM2_NAME)
  return(adm1_adm2_list)
}


#' @title Extract shapefile for a country
#'
#' @description Extract shapefile for a country
#' p_gishub: path to the shapefile from the GISHub
#' iso3: ISO3 code of the country
#' p_out: path to the output shapefile
#' @export
#' @examples
#' extract_shp_from_gishub(
#'   p_gishub = p_shapefile_adm2,
#'   iso3 = 'COG',
#'   p_out = p_congo_shp
#' )
#' congo_shp <- st_read(p_congo_shp)
#' plot(congo_shp)
#' @author Chunzhe ZHANG
#' @importFrom sf st_read st_write
#' @importFrom dplyr filter
#' @importFrom rlang .data
#' @rdname gis
extract_shp_from_gishub <- function(p_gishub, iso3, p_out) {
  gishub <- st_read(p_gishub)
  gishub |>
    filter(.data$ISO_3_CODE == iso3) |>
    st_write(p_out)
}
