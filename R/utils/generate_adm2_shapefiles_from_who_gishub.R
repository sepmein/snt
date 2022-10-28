
shp_regional_afro_adm2 <- file.path("inst/extdata/shapefiles/global/ADM2/AFRO/Detailed_Boundary_ADM2.shp")
shp_regional_amro_adm2 <- file.path("inst/extdata/shapefiles/global/ADM2/AMRO/Detailed_Boundary_ADM2.shp")
shp_regional_emro_adm2 <- file.path("inst/extdata/shapefiles/global/ADM2/EMRO/Detailed_Boundary_ADM2.shp")
shp_regional_euro_adm2 <- file.path("inst/extdata/shapefiles/global/ADM2/EURO/Detailed_Boundary_ADM2.shp")
shp_regional_searo_adm2 <- file.path("inst/extdata/shapefiles/global/ADM2/SEARO/Detailed_Boundary_ADM2.shp")
shp_regional_wpro_adm2 <- file.path("inst/extdata/shapefiles/global/ADM2/WPRO/Detailed_Boundary_ADM2.shp")

shp_regional_afro_adm2 <- sf::st_read(shp_regional_afro_adm2)
shp_regional_amro_adm2 <- sf::st_read(shp_regional_amro_adm2)
shp_regional_emro_adm2 <- sf::st_read(shp_regional_emro_adm2)
shp_regional_euro_adm2 <- sf::st_read(shp_regional_euro_adm2)
shp_regional_searo_adm2 <- sf::st_read(shp_regional_searo_adm2)
shp_regional_wpro_adm2 <- sf::st_read(shp_regional_wpro_adm2)

shp_global_adm2 <- dplyr::bind_rows(shp_regional_afro_adm2, shp_regional_amro_adm2) |>
    dplyr::bind_rows(shp_regional_emro_adm2) |>
    dplyr::bind_rows(shp_regional_emro_adm2) |>
    dplyr::bind_rows(shp_regional_euro_adm2) |>
    dplyr::bind_rows(shp_regional_searo_adm2) |>
    dplyr::bind_rows(shp_regional_wpro_adm2)

shp_global_adm2 |> sf::st_write("inst/extdata/shapefiles/global/ADM2/GLOBAL_ADM2.shp", layer_options = "ENCODING=UTF-8")
