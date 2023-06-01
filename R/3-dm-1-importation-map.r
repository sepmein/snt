#' @title Importation of data from MAP
#' @description This function is used to import data from MAP
#' @param ISO ISO code of the country, e.g. 'CIV'
#' @param layer Layer of the data
#' @importFrom malariaAtlas getShp getRaster
#' @export
#' @rdname MAP
sn_get_map_walk_healthcare <- function(iso, layer) {
  shp <- getShp(ISO = iso)
  access_walking <- getRaster(
    surface = "Walking-only travel time to healthcare map without access to motorized transport",
    shp = shp
  )
  return(access_walking)
}
#' @export
#' @rdname MAP
sn_get_map_motor_healthcare <- function(iso, layer) {
  shp <- getShp(ISO = iso)
  access_motor <- getRaster(
    surface = "Walking-only travel time to healthcare map without access to motorized transport",
    shp = shp
  )
  return(access_motor)
}
