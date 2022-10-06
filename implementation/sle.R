# reload package ----------------------------------------------------------


setwd("/Users/sepmein/dev/working/snt")
devtools::load_all()
library(snt)
library(tidyverse)
library(readxl)
library(haven)
library(tmap)


# meta data ---------------------------------------------------------------

set_country(
  root_folder = "/Users/sepmein/dev/working/snt-data",
  country = "SLE"
)
sle_adm2_shapefile <-
  "Countries/SLE/2022_malvac/shapefiles/sle_admn_adm2_py_who_district.shp"
sle_chiefdom_shapefile <-
  "Countries/SLE/2022_malvac/shapefiles/sle_admn_adm3_py_who_chiefdom.shp"
map_pfpr_2019 <- "Global/Data/MAP/2020_GBD2019_Global_PfPR_2019.tif"
ihme_all_cause_mortality <- "Global/Data/IHME/All-cause_mortality/mean/IHME_AFRICA_U5M_1998_2017_MEAN_UNDER5_2015_Y2017M09D25.TIF"
output_destination <- "Countries/SLE/2022_malvac/output"



# pfpr --------------------------------------------------------------------
sle_pfpr_adm3_2019 <- PfPrevalence$new(
  adm2_shapefile = sle_adm2_shapefile,
  adm3_shapefile = sle_chiefdom_shapefile,
  local_destination = map_pfpr_2019,
  output_destination = output_destination,
  with_95CI = FALSE
)

sle_pfpr_adm3_2019$load(
  target_adm_level = 3,
  adm2_name_in_shp = "New_Dist",
  adm3_name_in_shp = "New_Chief"
)

# TODO remove year
# TODO remove adm_name_in_shp
sle_pfpr_adm3_2019$plot_map(year = 2019,
                            adm2_name_in_shp = "New_Dist",
                            adm3_name_in_shp = "New_Chief",
                            adm1_name_in_shp = "PROVINCE",
                            # breaks = c(0,0.1, 0.2, 0.4, 1),
                            palette  = "Red-Green",
                            adm2_border_thickness = 2.5
                            )

sle_pfpr_adm2_2019 <- PfPrevalence$new(
  adm2_shapefile = sle_adm2_shapefile,
  adm3_shapefile = sle_chiefdom_shapefile,
  local_destination = map_pfpr_2019,
  output_destination = output_destination,
  with_95CI = FALSE
)

sle_pfpr_adm2_2019$load(
  target_adm_level = 2,
  adm2_name_in_shp = "New_Dist",
  adm3_name_in_shp = "New_Chief"
)

# TODO remove year
# TODO remove adm_name_in_shp
sle_pfpr_adm2_2019$plot_map(year = 2019,
                            adm2_name_in_shp = "New_Dist",
                            adm3_name_in_shp = "New_Chief",
                            adm1_name_in_shp = "PROVINCE",
                            # breaks = c(0,0.1, 0.2, 0.4, 1),
                            palette  = "Red-Green",
                            adm2_border_thickness = 2.5
)

# all cause mortality -----------------------------------------------------
ihme_all_mortality_u5_sle_adm2 <- IHME_mortality$new(
  adm2_shapefile = sle_adm2_shapefile,
  adm3_shapefile = sle_chiefdom_shapefile,
  local_destination = ihme_all_cause_mortality,
  output_destination = output_destination,
  with_95CI = FALSE
)

ihme_all_mortality_u5_sle_adm2$load(
  target_adm_level = 2,
  adm2_name_in_shp = "New_Dist",
  adm3_name_in_shp = "New_Chief"
)

ihme_all_mortality_u5_sle_adm2$export()

ihme_all_mortality_u5_sle_adm3 <- IHME_mortality$new(
  adm2_shapefile = sle_adm2_shapefile,
  adm3_shapefile = sle_chiefdom_shapefile,
  local_destination = ihme_all_cause_mortality,
  output_destination = output_destination,
  with_95CI = FALSE
)

ihme_all_mortality_u5_sle_adm3$load(
  target_adm_level = 3,
  adm2_name_in_shp = "New_Dist",
  adm3_name_in_shp = "New_Chief"
)

ihme_all_mortality_u5_sle_adm3$export(filename = "ihme_all_mortality_u5_sle_adm3.csv")

ihme_all_mortality_u5_sle_adm3$plot_map(
                            adm2_name_in_shp = "New_Dist",
                            adm3_name_in_shp = "New_Chief",
                            adm1_name_in_shp = "PROVINCE",
                            # breaks = c(0,0.1, 0.2, 0.4, 1),
                            palette  = "Red-Green",
                            adm2_border_thickness = 2.5
)
# monthly data ------------------------------------------------------------
sle_monthly <-
  smart_get_all_files_in_dir(
    smart_path = "Countries/SLE/2022_malvac/districts",
    skip = 0,
    clean = TRUE,
    country = "SLE"
  )

# fix the month data type as character
sle_monthly$data[[8]] <-
  sle_monthly$data[[8]] |> mutate(month = as.character(month))
sle_monthly$data[[13]] <-
  sle_monthly$data[[13]] |> mutate(month = as.character(month))
sle_monthly$data[[15]] <-
  sle_monthly$data[[15]] |> mutate(month = as.character(month))
sle_monthly$data[[16]] <-
  sle_monthly$data[[16]] |> mutate(month = as.character(month))

sle_monthly <- sle_monthly |> unnest(data)

sle_monthly <- sle_monthly |>
  mutate(
    alladm_ov5 = across(
      starts_with("alladm_ov5"),
      ~ sum(.x, na.rm = TRUE)
    ),
    alladm_ov5 = alladm_ov5 + maladm_ov5_1 + maladm_ov5_2 + maladm_u5,
    maladm_ov5 = across(
      starts_with("maladm_ov5"),
      ~ sum(.x, na.rm = TRUE)
    ),
    alldth_ov5 = across(
      starts_with("alldth_ov5"),
      ~ sum(.x, na.rm = TRUE)
    ),
    susp_ov5 = across(
      starts_with("susp_ov5"),
      ~ sum(.x, na.rm = TRUE)
    ),
    test_mic_u5 = across(
      starts_with("test_mic_u5"),
      ~ sum(.x, na.rm = TRUE)
    ),
    test_mic_ov5 = across(
      starts_with("test_mic_ov5"),
      ~ sum(.x, na.rm = TRUE)
    ),
    test_rdt_u5 = across(
      starts_with("test_rdt_u5"),
      ~ sum(.x, na.rm = TRUE)
    ),
    test_rdt_ov5 = across(
      starts_with("test_rdt_ov5"),
      ~ sum(.x, na.rm = TRUE)
    ),
    maltreat_u5 = across(
      starts_with("maltreat_u5"),
      ~ sum(.x, na.rm = TRUE)
    ),
    maltreat_ov5 = across(
      starts_with("maltreat_ov5"),
      ~ sum(.x, na.rm = TRUE)
    ),
    penta3_tot = across(
      starts_with("penta3_tot"),
      ~ sum(.x, na.rm = TRUE)
    ),
    susp = susp_u5 + susp_ov5,
    test_u5 = test_mic_u5 + test_rdt_u5,
    test_ov5 = test_mic_ov5 + test_rdt_ov5,
    conf_mic_u5 = test_mic_u5_p,
    conf_mic_ov5 = test_mic_ov5_1_p + test_mic_ov5_2_p,
    conf_mic = conf_mic_u5 + conf_mic_ov5,
    conf_rdt_u5 = test_rdt_u5_p,
    conf_rdt_ov5 = test_rdt_ov5_1_p + test_rdt_ov5_2_p,
    conf_rdt = conf_rdt_u5 + conf_rdt_ov5,
    conf_u5 = conf_mic_u5 + conf_rdt_u5,
    conf_ov5 = conf_mic_ov5 + conf_rdt_ov5,
    maladm = maladm_u5 + maladm_ov5,
    maldth_ov5 = maldth_ov5_1 + maldth_ov5_2,
    maldth = maldth_u5 + maldth_ov5,
  )

routine_data <- sle_monthly |>
  mutate(adm1 = "SLE") |>
  select(
    adm1,
    adm2,
    hf_name,
    year,
    month,
    allout_u5,
    allout_ov5,
    allout,
    alladm_u5,
    alladm_ov5,
    alladm,
    alldth_u5,
    alldth_ov5,
    alldth,
    susp_u5,
    susp_ov5,
    susp,
    pres_u5,
    pres_ov5,
    pres,
    test_mic_u5,
    test_mic_ov5,
    test_mic,
    test_rdt_u5,
    test_rdt_ov5,
    test_rdt,
    test_u5,
    test_ov5,
    test,
    conf_mic_u5,
    conf_mic_ov5,
    conf_mic,
    conf_rdt_u5,
    conf_rdt_ov5,
    conf_rdt,
    conf_u5,
    conf_ov5,
    conf,
    maladm_u5,
    maladm_ov5,
    maladm,
    maldth_u5,
    maldth_ov5,
    maldth
  )

# reports rates -----------------------------------------------------------
