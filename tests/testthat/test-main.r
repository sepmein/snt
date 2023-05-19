test_that("raster resource load ", {
  p_adm2_shp <-
    file.path(proj_get(), "tests/mock/shapefile/civ_adm2_shapefile.shp")

  ## rainfall ------
  p_chirps_rainfall_raster <-
    file.path(proj_get(), "tests/mock/raster/")

  # Extract rainfall data -----
  rainfall <- snt::RasterResource$new(
    adm2_shapefile = p_adm2_shp,
    local_destination = p_chirps_rainfall_raster,
    output_destination = ".",
    index_name = "rainfall"
  )

  rainfall$load(
    target_adm_level = 2,
    adm2_name_in_shp = "NAME_2"
  )

  expect_equal(
    nrow(rainfall$data |> select(NAME_2) |>
      distinct()),
    nrow(sf::st_read(p_adm2_shp))
  )
})
