# load library -----
devtools::load_all()
library(snt)
library(sf)

shp_global_adm0 <- file.path(
    "inst/extdata/shapefiles/global/ADM0/GLOBAL_ADM0.shp"
)
shp_global_adm1 <- file.path(
    "inst/extdata/shapefiles/global/ADM1/Detailed_Boundary_ADM1.shp"
)
shp_global_adm2 <- file.path(
    "inst/extdata/shapefiles/global/ADM2/GLOBAL_ADM2.shp"
)
path_rainfall <- file.path(
    "/Users/sepmein/x/snt-data/Global/Data/CHIRPS_Global_raster_files"
)
# rainfall resource ------
# example rainfall database
#
resource_rainfall_adm0 <- RasterResource$new(
    adm0_shapefile = shp_global_adm0,
    adm1_shapefile = shp_global_adm1,
    adm2_shapefile = shp_global_adm2,
    local_destination = path_rainfall,
    output_destination = NULL,
    local_file_type = "raster",
    index_name = "rainfall"
)

resource_rainfall_adm0$load(
    target_adm_level = 0
)
resource_rainfall_adm0$data |>
    mutate(
        year = stringr::str_extract(file, "\\d{4}"),
        month = stringr::str_extract(file, "\\d{2}(?=.tif)")
    ) |>
    select(GUID, year, month, value) |>
    rename(rainfall = value) |>
    st_drop_geometry() |>
    tibble::as_tibble() |>
    readr::write_csv("rainfall_adm0.csv")

resource_rainfall_adm1 <- RasterResource$new(
    adm0_shapefile = shp_global_adm0,
    adm1_shapefile = shp_global_adm1,
    adm2_shapefile = shp_global_adm2,
    local_destination = path_rainfall,
    output_destination = NULL,
    local_file_type = "raster",
    index_name = "rainfall"
)

resource_rainfall_adm1$load(
    target_adm_level = 1
)

resource_rainfall_adm1$data |>
    mutate(
        year = stringr::str_extract(file, "\\d{4}"),
        month = stringr::str_extract(file, "\\d{2}(?=.tif)")
    ) |>
    select(GUID, year, month, value) |>
    rename(rainfall = value) |>
    st_drop_geometry() |>
    as_tibble() |>
    write_csv("rainfall_adm1.csv")

resource_rainfall_adm2 <- RasterResource$new(
    adm0_shapefile = shp_global_adm0,
    adm1_shapefile = shp_global_adm1,
    adm2_shapefile = shp_global_adm2,
    local_destination = path_rainfall,
    output_destination = NULL,
    local_file_type = "raster",
    index_name = "rainfall"
)

resource_rainfall_adm2$load(
    target_adm_level = 2
)

resource_rainfall_adm2$data |>
    mutate(
        year = stringr::str_extract(file, "\\d{4}"),
        month = stringr::str_extract(file, "\\d{2}(?=.tif)")
    ) |>
    select(GUID, year, month, value) |>
    rename(rainfall = value) |>
    st_drop_geometry() |>
    as_tibble() |>
    write_csv("rainfall_adm2.csv")
