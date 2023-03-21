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
#'   iso3 = "COG",
#'   p_out = p_congo_shp
#' )
#' congo_shp <- st_read(p_congo_shp)
#' plot(congo_shp)
#' @author Chunzhe ZHANG
extract_shp_from_gishub <- function(p_gishub, iso3, p_out) {
  gishub <- st_read(p_gishub)
  gishub |>
    filter(.data$ISO_3_CODE == iso3) |>
    st_write(p_out)
}
