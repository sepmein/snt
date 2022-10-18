# reload package ----------------------------------------------------------


setwd("../snt")
devtools::load_all()
library(snt)
library(tidyverse)
library(readxl)
library(haven)
library(tmap)
tmap_options(check.and.fix = TRUE)

# meta data ---------------------------------------------------------------

set_country(root_folder = "../snt-data",
            country = "SLE")
sle_adm2_shapefile <-
  "Countries/SLE/2022_malvac/shapefiles/sle_admn_adm2_py_who_district.shp"
sle_chiefdom_shapefile <-
  "Countries/SLE/2022_malvac/shapefiles/sle_admn_adm3_py_who_chiefdom.shp"
map_pfpr_2019 <- "Global/Data/MAP/2020_GBD2019_Global_PfPR_2019.tif"
ihme_all_cause_mortality <-
  "Global/Data/IHME/All-cause_mortality/mean/IHME_AFRICA_U5M_1998_2017_MEAN_UNDER5_2015_Y2017M09D25.TIF"
output_destination <- "Countries/SLE/2022_malvac/output"


## read pop
## read mis_report
treatment_seeking_adm <-
  read_dta("Countries/SLE/2022_malvac/treatment_seeking_adm1.dta")

## read adm1 adm2 adm3 data from shapefile
sle_adm <- sf::st_read(sle_chiefdom_shapefile)
sle_adm <- as_tibble(sle_adm) |>
  select(PROVINCE, New_Dist, New_Chief) |>
  rename(adm1 = PROVINCE,
         adm2 = New_Dist,
         adm3 = New_Chief)

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
sle_pfpr_adm3_2019$plot_map(
  year = 2019,
  adm2_name_in_shp = "New_Dist",
  adm3_name_in_shp = "New_Chief",
  adm1_name_in_shp = "PROVINCE",
  breaks = c(0, 0.1, 0.2, 0.4, 1),
  palette = c("#d5d5d5", "#8fbfe5", "#fbe484", "#782456"),
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
sle_pfpr_adm2_2019$plot_map(
  year = 2019,
  adm2_name_in_shp = "New_Dist",
  adm3_name_in_shp = "New_Chief",
  adm1_name_in_shp = "PROVINCE",
  breaks = c(0, 0.1, 0.2, 0.4, 1),
  palette = c("#d5d5d5", "#8fbfe5", "#fbe484", "#782456"),
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


ihme_all_mortality_u5_sle_adm2$plot_map(
  adm2_name_in_shp = "New_Dist",
  adm3_name_in_shp = "New_Chief",
  adm1_name_in_shp = "PROVINCE",
  breaks = c(0.06, 0.075, 0.095, 1),
  palette = c("#fbe484", "#ed9c4a", "#ea3423"),
  adm2_border_thickness = 2.5
)

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
  breaks = c(0.06, 0.075, 0.095, 1),
  palette = c("#fbe484", "#ed9c4a", "#ea3423"),
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

## Clean ----
# fix the naming of adm2 and adm3 manually
# fix the month data type as character
sle_monthly$data[[8]] <-
  sle_monthly$data[[8]] |> mutate(month = as.character(month))
sle_monthly$data[[13]] <-
  sle_monthly$data[[13]] |> mutate(month = as.character(month))
sle_monthly$data[[15]] <-
  sle_monthly$data[[15]] |> mutate(month = as.character(month))
sle_monthly$data[[16]] <-
  sle_monthly$data[[16]] |> mutate(month = as.character(month))

sle_monthly <- sle_monthly |>
  unnest(data) |>
  mutate_if(is.numeric, replace_na, replace = 0)

sle_monthly <- sle_monthly |>
  mutate(
    alladm_ov5_7 = as.numeric(alladm_ov5_7),
    alldth_ov5_4 = as.numeric(alldth_ov5_4),
    alldth_ov5_5 = as.numeric(alldth_ov5_5),
    alldth_ov5_11 = as.numeric(alldth_ov5_11),
    alladm_ov5_14 = as.numeric(alladm_ov5_14),
    alldth_ov5_12 = as.numeric(alldth_ov5_12),
    alldth_ov5_13 = as.numeric(alldth_ov5_13)
  )

## Calculate Index ----
sle_monthly <- sle_monthly |>
  mutate(
    allout = allout_u5 + allout_ov5,
    alladm_ov5 = rowSums(across(starts_with("alladm_ov5")), na.rm = TRUE),
    alladm_ov5 = alladm_ov5 + maladm_ov5_1 + maladm_ov5_2 + maladm_u5,
    alladm = alladm_u5 + alladm_ov5,
    maladm_ov5 = rowSums(across(starts_with("maladm_ov5")), na.rm = TRUE),
    alldth_ov5 = rowSums(across(starts_with("alldth_ov5")), na.rm = TRUE),
    alldth = alldth_u5 + alldth_ov5,
    susp_ov5 = rowSums(across(starts_with("susp_ov5")), na.rm = TRUE),
    susp = susp_u5 + susp_ov5,
    test_mic_u5 = rowSums(across(starts_with("test_mic_u5")), na.rm = TRUE),
    test_mic_ov5 = rowSums(across(starts_with("test_mic_ov5")), na.rm = TRUE),
    test_mic = test_mic_u5 + test_mic_ov5,
    test_rdt_u5 = rowSums(across(starts_with("test_rdt_u5")), na.rm = TRUE),
    test_rdt_ov5 = rowSums(across(starts_with("test_rdt_ov5")), na.rm = TRUE),
    test_rdt = test_rdt_u5 + test_rdt_ov5,
    test_u5 = test_mic_u5 + test_rdt_u5,
    test_ov5 = test_mic_ov5 + test_rdt_ov5,
    test = test_u5 + test_ov5,
    maltreat_u5 = rowSums(across(starts_with("maltreat_u5")), na.rm = TRUE),
    maltreat_ov5 = rowSums(across(starts_with("maltreat_ov5")), na.rm = TRUE),
    penta3_tot = rowSums(across(starts_with("penta3_tot")), na.rm = TRUE),
    mr1_tot = rowSums(across(starts_with("mr1_tot")), na.rm = TRUE),
    conf_mic_u5 = test_mic_u5_p,
    conf_mic_ov5 = test_mic_ov5_1_p + test_mic_ov5_2_p,
    conf_mic = conf_mic_u5 + conf_mic_ov5,
    conf_rdt_u5 = test_rdt_u5_p,
    conf_rdt_ov5 = test_rdt_ov5_1_p + test_rdt_ov5_2_p,
    conf_rdt = conf_rdt_u5 + conf_rdt_ov5,
    conf_u5 = conf_mic_u5 + conf_rdt_u5,
    conf_ov5 = conf_mic_ov5 + conf_rdt_ov5,
    conf = conf_mic + conf_rdt,
    maltreat = maltreat_u5 + maltreat_ov5,
    maladm = maladm_u5 + maladm_ov5,
    maldth_ov5 = maldth_ov5_1 + maldth_ov5_2,
    maldth = maldth_u5 + maldth_ov5,
  )

## Adm1 info ----
sle_monthly <-
  sle_monthly |> left_join(sle_adm |> select(adm1, adm2) |> distinct(adm1, adm2))
## Treatment seeking ----
sle_monthly <- sle_monthly |> left_join(treatment_seeking_adm)

## Population --------------------------------------------------------------
pop <- readxl::read_xlsx(path = "SLE_pop.xlsx")
sle_monthly <- sle_monthly |> left_join(pop)

# immunization follow up rates

immunization_follow_up <- sle_monthly |>
  select(
    adm1,
    adm2,
    adm3,
    penta3_tot,
    mr1_tot
  ) |>
  group_by(
    adm2
  ) |>
  summarise(penta3_tot = sum(penta3_tot, na.rm = TRUE), mr1_tot = sum(mr1_tot, na.rm = TRUE))

write_csv(immunization_follow_up, "SLE_adm2_immunization_follow_up.csv")
  
## Sle monthly
# sle_adm_monthly <-
# sle_monthly |> group_by(adm2, adm3) |> summarise(n = n())
# write_csv(sle_adm_monthly, 'sle_adm_monthly.csv')

# Routine data -----
routine_data <- sle_monthly |>
  select(
    adm1,
    adm2,
    adm3,
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
    maltreat_u5,
    maltreat_ov5,
    maltreat,
    maladm_u5,
    maladm_ov5,
    maladm,
    maldth_u5,
    maldth_ov5,
    maldth,
    pub_treat,
    priv_treat,
    no_treat,
    pop2022
  ) |>
  rename(hf = hf_name)

## Presumed Cases ----
routine_data <- routine_data |> mutate(
  pres_u5 = if_else(maltreat_u5 - conf_u5 >= 0, maltreat_u5 - conf_u5, conf_u5),
  pres_ov5 = if_else(maltreat_ov5 - conf_ov5 >= 0, maltreat_ov5 - conf_ov5, conf_ov5),
  pres = pres_u5 + pres_ov5
)


write.csv(routine_data, "SLE_routinedata_hf_month.csv")

### 2.3 Outliers ------------------------------------------------------------

#### 2.3.1 Plot --------------------------------------------------------------------
routine_data |>
  select(year,
         allout_ov5,
         allout_u5,
         allout) |>
  pivot_longer(cols = 2:4,
               names_to = "index",
               values_to = "value") |>
  mutate(year = factor(year)) |>
  ggplot(aes(x = value, y = index)) +
  # geom_boxplot() +
  geom_point(aes(color = year), alpha = 0.5)
#
# routine_data |>
#   select(year,
#          allout_ov5,
#          allout_u5,
#          allout) |>
#   pivot_longer(cols = 2:4,
#                names_to = "index",
#                values_to = "value") |>
#   mutate(year= factor(year)) |>
#   ggstatsplot::ggbetweenstats(x = index,
#                               y = value,
#                               pairwise.comparisons = FALSE)
ggsave(
  "SLE_outliers_1.png",
  width = 5,
  height = 4,
  dpi = 300
)

routine_data |>
  select(year,
         maldth_ov5,
         maldth_u5,
         maldth,
         maltreat_ov5,
         maltreat_u5,
         maltreat,
         conf,
         test) |>
  pivot_longer(cols = 2:9,
               names_to = "index",
               values_to = "value") |>
  mutate(year = factor(year)) |>
  ggplot(aes(x = value, y = index)) +
  # geom_boxplot() +
  geom_point(aes(color = year), alpha = 0.5)
# mutate(year= factor(year)) |>
# ggplot(aes(x = value, y = index)) +
# geom_boxplot() +
# # geom_point(aes(shape = year),alpha = 0.01)
# ggstatsplot::ggbetweenstats(x = index,
#                             y = value,
#                             pairwise.comparisons = FALSE)
ggsave(
  "SLE_outliers_2.png",
  width = 5,
  height = 4,
  dpi = 300
)

routine_data |>
  select(year,
         maladm,
         maladm_ov5,
         maladm_u5,
         alladm,
         alladm_ov5,
         alladm_u5,
  ) |>
  pivot_longer(cols = 2:7,
               names_to = "index",
               values_to = "value") |>
  # mutate(year= factor(year)) |>
  # ggplot(aes(x = value, y = index)) +
  # geom_boxplot() +
  # geom_point(aes(shape = year),alpha = 0.01)
  mutate(year = factor(year)) |>
  ggplot(aes(x = value, y = index)) +
  # geom_boxplot() +
  geom_point(aes(color = year), alpha = 0.5)
# ggstatsplot::ggbetweenstats(x = index,
#                             y = value,
#                             pairwise.comparisons = FALSE)
ggsave(
  "SLE_outliers_3.png",
  width = 5,
  height = 4,
  dpi = 300
)

#### 2.3.2 List --------------------------------------------------------------------
outliers <- find_outlier(routine_data)
outliers_by_hf <- outliers_find_hf(outliers)
# per health facilities
# outlier_test <- EnvStats::rosnerTest(routine_monthly$maldth, 10000)
# outlier_test$all.stats
### 2.4 Consistency test --------------------------------------------------------

consistency_color <- c("Blue", "Red")

routine_data |>
  mutate(error = if_else(test > allout, TRUE, FALSE)) |>
  ggplot() +
  geom_point(aes(y = allout, x = test, color = error)) +
  scale_color_manual("test > allout", values = consistency_color) +
  geom_abline(slope = 1)
ggsave("SLE_consistency_1.png")

routine_data |>
  mutate(error = if_else(conf > test, TRUE, FALSE)) |>
  ggplot() +
  geom_point(aes(y = test, x = conf, color = error)) +
  scale_color_manual("conf > test", values = consistency_color) +
  geom_abline(slope = 1)
ggsave("SLE_consistency_2.png")

routine_data |>
  mutate(error = if_else(maltreat > conf, TRUE, FALSE)) |>
  ggplot() +
  geom_point(aes(y = conf, x = maltreat, color = error)) +
  scale_color_manual("maltreat > conf", values = consistency_color) +
  geom_abline(slope = 1)
ggsave("SLE_consistency_3.png")

routine_data |>
  mutate(error = if_else(maldth > maladm, TRUE, FALSE)) |>
  routine_data |>
  ggplot() +
  geom_point(aes(y = maladm, x = maldth, color = error)) +
  scale_color_manual("maldth > maladm", values = consistency_color) +
  geom_abline(slope = 1)
ggsave("SLE_consistency_4.png")

routine_data |>
  mutate(error = if_else(maldth > alldth, TRUE, FALSE)) |>
  ggplot() +
  geom_point(aes(y = alldth, x = maldth, color = error)) +
  scale_color_manual("maldth > alldth", values = consistency_color) +
  geom_abline(slope = 1)
ggsave("SLE_consistency_5.png")

routine_data |>
  mutate(error = if_else(maladm > alladm, TRUE, FALSE)) |>
  ggplot() +
  geom_point(aes(y = alladm, x = maladm, color = error)) +
  scale_color_manual("maladm > alladm", values = consistency_color) +
  geom_abline(slope = 1)
ggsave("SLE_consistency_6.png")

# Incidence -----------------------------------------------------------
## By Adm2 -----
### By Month -----

#### report rates by adm2 month -----
adm2_reprat_month <- routine_data |>
  mutate(reported = if_else(conf > 0, 1, 0)) |>
  group_by(adm2, hf, year, month) |>
  summarise(reported = sum(reported, na.rm = TRUE),
            n_hf = n(),
  ) |>
  group_by(adm2, year, month) |>
  summarise(reprat = sum(reported) / sum(n_hf))

write.csv(adm2_reprat_month, "reprat_adm2_month.csv")

#### N1 -----
cases_crude <- routine_data |>
  mutate(n1 = conf + susp * conf / test) |>
  group_by(adm2, year, month) |>
  summarise(
    n1 = sum(n1, na.rm = TRUE),
    conf = sum(conf, na.rm = TRUE),
    susp = sum(susp, na.rm = TRUE),
    test = sum(test, na.rm = TRUE),
    pub_treat = mean(pub_treat),
    priv_treat = mean(priv_treat),
    no_treat = mean(no_treat),
    pop2022 = mean(pop2022)
  )

#### N2 -----
n2 <- cases_crude |>
  left_join(adm2_reprat_month) |>
  mutate(n2 = n1 / reprat)

#### N3 -----
n3 <-
  n2 |> mutate(n3 = n2 + n2 * priv_treat / pub_treat + n2 * no_treat / pub_treat)


### report rates by adm2 year -----
adm2_reprat_year <- routine_data |>
  mutate(reported = if_else(conf > 0, 1, 0)) |>
  group_by(adm2, hf, year) |>
  summarise(reported = sum(reported, na.rm = TRUE),
            n_hf = n()) |>
  group_by(adm2, year) |>
  summarise(reprat = sum(reported) / sum(n_hf))
write.csv(adm2_reprat_year, "reprat_adm2_year.csv")


#### N1 -----
cases_crude_adm2_year <- routine_data |>
  mutate(n1 = conf + susp * conf / test) |>
  group_by(adm2, year) |>
  summarise(
    n1 = sum(n1, na.rm = TRUE),
    conf = sum(conf, na.rm = TRUE),
    susp = sum(susp, na.rm = TRUE),
    test = sum(test, na.rm = TRUE),
    pub_treat = mean(pub_treat),
    priv_treat = mean(priv_treat),
    no_treat = mean(no_treat),
    pop2022 = mean(pop2022)
  )

#### N2 -----
n2_adm2_year <-
  cases_crude_adm2_year |>
  left_join(adm2_reprat_year) |>
  mutate(n2 = n1 / reprat)

#### N3 -----
n3_adm2_year <- n2_adm2_year |>
  mutate(n3 = n2 + n2 * priv_treat / pub_treat + n2 * no_treat / pub_treat)


incidence_adm2_year <- n3_adm2_year |> mutate(
  incidence_crude = conf / pop2022 * 1000,
  incidence_n1 = n1 / pop2022 * 1000,
  incidence_n2 = n2 / pop2022 * 1000,
  incidence_n3 = n3 / pop2022 * 1000
)

write_csv(incidence_adm2_year, "SLE_incidence_adm2.csv")

## By Adm3 -----

### By Month -----

#### report rates by adm3 month -----
adm3_reprat_month <- routine_data |>
  mutate(reported = if_else(conf > 0, 1, 0)) |>
  group_by(adm3, hf, year, month) |>
  summarise(reported = sum(reported, na.rm = TRUE),
            n_hf = n()) |>
  group_by(adm3, year, month) |>
  summarise(reprat = sum(reported) / sum(n_hf))

write.csv(adm3_reprat_month, "reprat_adm3_month.csv")

#### N1 -----
cases_crude_adm3_month <- routine_data |>
  mutate(n1 = conf + susp * conf / test) |>
  group_by(adm3, year, month) |>
  summarise(
    n1 = sum(n1, na.rm = TRUE),
    conf = sum(conf, na.rm = TRUE),
    susp = sum(susp, na.rm = TRUE),
    test = sum(test, na.rm = TRUE),
    pub_treat = mean(pub_treat),
    priv_treat = mean(priv_treat),
    no_treat = mean(no_treat)
  )

#### N2 -----
n2_adm3_month <- cases_crude_adm3_month |>
  left_join(adm3_reprat_month) |>
  mutate(n2 = n1 / reprat)

#### N3 -----
n3_adm3_month <-
  n2_adm3_month |>
  mutate(n3 = n2 + n2 * priv_treat / pub_treat + n2 * no_treat / pub_treat)

### report rates by adm3 year -----
adm3_reprat_year <- routine_data |>
  mutate(reported = if_else(conf > 0, 1, 0)) |>
  group_by(adm3, hf, year) |>
  summarise(reported = sum(reported, na.rm = TRUE),
            n_hf = n()) |>
  group_by(adm3, year) |>
  summarise(reprat = sum(reported) / sum(n_hf))
write.csv(adm3_reprat_year, "reprat_adm3_year.csv")


#### N1 -----
cases_crude_adm3_year <- routine_data |>
  mutate(n1 = conf + susp * conf / test) |>
  group_by(adm3, year) |>
  summarise(
    n1 = sum(n1, na.rm = TRUE),
    conf = sum(conf, na.rm = TRUE),
    susp = sum(susp, na.rm = TRUE),
    test = sum(test, na.rm = TRUE),
    pub_treat = mean(pub_treat),
    priv_treat = mean(priv_treat),
    no_treat = mean(no_treat),
    pop2022 = mean(pop2022)
  )

#### N2 -----
n2_adm3_year <-
  cases_crude_adm3_year |>
  left_join(adm3_reprat_year) |>
  mutate(n2 = n1 / reprat)

#### N3 -----
n3_adm3_year <- n2_adm3_year |>
  mutate(n3 = n2 + n2 * priv_treat / pub_treat + n2 * no_treat / pub_treat)
write_csv(n3_adm3_year, "SLE_incidence_adm3.csv")

# Plot Incidence ----------------------------------------------------------

sle_adm2_map <- sf::st_read(sle_adm2_shapefile)

sle_adm2_incidence_map_and_data <-
  sle_adm2_map |> left_join(incidence_adm2_year,
                            by = c("New_Dist" = "adm2"))

## By adm2 year ------------------------------------------------------------

tmap_mode("plot")
tmap_options(check.and.fix = TRUE)
incidence_crude_map_adm2 <-
  tm_shape(sle_adm2_incidence_map_and_data) +
  tm_polygons(
    col = "incidence_crude",
    breaks = c(0, 250, 350, 450, 20000),
    palette = grDevices::hcl.colors(4,
                                    "Blue-Red")
  ) +
  tmap::tm_facets(by = "year")
print(incidence_crude_map_adm2)
tmap_save(incidence_crude_map_adm2, "sle_incidence_crude_adm2.png")

incidence_n1_map_adm2 <-
  tm_shape(sle_adm2_incidence_map_and_data) +
  tm_polygons(
    col = "incidence_n1",
    breaks = c(0, 250, 350, 450, 20000),
    palette = grDevices::hcl.colors(4,
                                    "Blue-Red")
  ) +
  tmap::tm_facets(by = "year")
print(incidence_n1_map_adm2)
tmap_save(incidence_n1_map_adm2, "sle_incidence_n1_adm2.png")


incidence_n2_map_adm2 <-
  tm_shape(sle_adm2_incidence_map_and_data) +
  tm_polygons(
    col = "incidence_n2",
    breaks = c(0, 250, 350, 450, 20000),
    palette = grDevices::hcl.colors(4,
                                    "Blue-Red")
  ) +
  tmap::tm_facets(by = "year")
print(incidence_n2_map_adm2)
tmap_save(incidence_n2_map_adm2, "sle_incidence_n2_adm2.png")


incidence_n3_map_adm2 <-
  tm_shape(sle_adm2_incidence_map_and_data) +
  tm_polygons(
    col = "incidence_n3",
    breaks = c(0, 250, 350, 450, 20000),
    palette = grDevices::hcl.colors(4,
                                    "Blue-Red")
  ) +
  tmap::tm_facets(by = "year")
print(incidence_n3_map_adm2)
tmap_save(incidence_n3_map_adm2, "sle_incidence_n3_adm2.png")

## By adm3 year ------------------------------------------------------------
sle_adm3_map <- sf::st_read(sle_chiefdom_shapefile)

sle_adm3_incidence_map_and_data <-
  sle_adm3_map |> left_join(incidence_adm3_year,
                            by = c("ChiefDom" = "adm3"))

tmap_mode("plot")
tmap_options(check.and.fix = TRUE)
incidence_crude_map_adm3 <-
  tm_shape(sle_adm3_incidence_map_and_data) +
  tm_polygons(
    col = "incidence_crude",
    breaks = c(0, 250, 350, 450, 20000),
    palette = grDevices::hcl.colors(4,
                                    "Blue-Red")
  ) +
  tmap::tm_facets(by = "year")
print(incidence_crude_map_adm3)
tmap_save(incidence_crude_map_adm3, "sle_incidence_crude_adm3.png")

incidence_n1_map_adm3 <-
  tm_shape(sle_adm3_incidence_map_and_data) +
  tm_polygons(
    col = "incidence_n1",
    breaks = c(0, 250, 350, 450, 20000),
    palette = grDevices::hcl.colors(4,
                                    "Blue-Red")
  ) +
  tmap::tm_facets(by = "year")
print(incidence_n1_map_adm3)
tmap_save(incidence_n1_map_adm3, "sle_incidence_n1_adm3.png")


incidence_n2_map_adm3 <-
  tm_shape(sle_adm3_incidence_map_and_data) +
  tm_polygons(
    col = "incidence_n2",
    breaks = c(0, 250, 350, 450, 20000),
    palette = grDevices::hcl.colors(4,
                                    "Blue-Red")
  ) +
  tmap::tm_facets(by = "year")
print(incidence_n2_map_adm3)
tmap_save(incidence_n2_map_adm3, "sle_incidence_n2_adm3.png")


incidence_n3_map_adm3 <-
  tm_shape(sle_adm3_incidence_map_and_data) +
  tm_polygons(
    col = "incidence_n3",
    breaks = c(0, 250, 350, 450, 20000),
    palette = grDevices::hcl.colors(4,
                                    "Blue-Red")
  ) +
  tmap::tm_facets(by = "year")
print(incidence_n3_map_adm3)
tmap_save(incidence_n3_map_adm3, "sle_incidence_n3_adm3.png")


# Missing check -----
## 2.4.1 Plot by HF & Date -------------------------------------------------------
report_status_by_hf_and_date <- routine_data |>
  group_by(adm1, adm2, adm3, hf, year, month) |>
  summarise(sum = sum(c_across(allout_u5:maldth), na.rm = TRUE)) |>
  mutate(reported = if_else(sum > 0, "Y", "N"),
         yearmon = str_c(year, str_pad(month, 2, pad = "0"))) |>
  select(adm1, adm2, adm3, hf, yearmon, reported)

write_csv(report_status_by_hf_and_date, "SLE_hfmonth_activity.csv")

report_status_by_hf_and_date |>
  ggplot(aes(x = yearmon, y = hf, fill = reported)) +
  geom_tile() +
  scale_x_discrete(guide = guide_axis(check.overlap = TRUE)) +
  labs(title = "Report Status By Health Facilities and Date",
       y = "Health Facilities",
       x = "Date") +
  theme(axis.text.y = element_blank()) +
  scale_fill_manual(values = c("N" = "white", "Y" = "red3"))
ggsave(
  filename = "report_status_by_hf_and_date.png",
  width = 7,
  height = 5,
)
## 2.4.4 Plot Indicators & Date --------------------------------------------------
report_status_by_indicators_and_date <- routine_data |>
  pivot_longer(
    cols = c(
      "test",
      "pres",
      "maltreat",
      "maldth",
      "maladm",
      "conf",
      "allout",
    ),
    names_to = "index",
    values_to = "value"
  ) |>
  mutate(reported = if_else(value > 0, 1, 0)) |>
  group_by(index, year) |>
  summarise(reports = sum(reported, na.rm = TRUE), n = n()) |>
  mutate(report_rate = reports / n)

ggplot(report_status_by_indicators_and_date) +
  geom_tile(aes(x = year, y = index, fill = report_rate)) +
  scale_fill_viridis_c()

ggsave(filename = "report_status_by_indicators_and_date.png",
       width = 7,
       height = 6)

## 2.4.5 Plot By ADM2 and Date ---------------------------------------------------
report_status_by_adm2_and_date <- routine_data |>
  pivot_longer(cols = susp:maldth,
               names_to = "index",
               values_to = "value") |>
  mutate(reported = if_else(value > 0, 1, 0),
         yearmon = str_c(year, str_pad(month, 2, pad = "0"))) |>
  group_by(adm2, yearmon) |>
  summarise(reports = sum(reported, na.rm = TRUE), n = n()) |>
  mutate(report_rate = reports / n)

ggplot(report_status_by_adm2_and_date) +
  geom_tile(aes(x = yearmon, y = adm2, fill = report_rate)) +
  scale_fill_viridis_c() +
  scale_x_discrete(guide = guide_axis(check.overlap = TRUE)) +
  labs(title = "Report Status By Adm2 and Date",
       y = "Districts",
       x = "Date")

# Categorization -----

categorization <- function(PfPR, au5mr_cat) {
  if ((PfPR >= 0.2 & PfPR <= 0.4) & (au5mr_cat >= 0.095)) {
    cat <- 1
  } else if ((PfPR >= 0.4) & (au5mr_cat >= 0.095)) {
    cat <- 1
  } else if (PfPR >= 0.4 &
             (au5mr_cat >= 0.075 & au5mr_cat < 0.095)) {
    cat <- 1
  } else if ((PfPR >= 0.1 & PfPR <= 0.2) & au5mr_cat >= 0.095) {
    cat <- 2
  } else if ((PfPR >= 0.2 & PfPR <= 0.4) &
             (au5mr_cat >= 0.075 & au5mr_cat < 0.095)) {
    cat <- 2
  } else if (PfPR >= 0.4 &
             (au5mr_cat >= 0.06 & au5mr_cat < 0.075)) {
    cat <- 2
  } else if ((PfPR >= 0.1 &
              PfPR <= 0.2) &
             (au5mr_cat >= 0.075 & au5mr_cat < 0.095)) {
    cat <- 3
  } else if ((PfPR >= 0.2 &
              PfPR <= 0.4) &
             (au5mr_cat >= 0.06 & au5mr_cat < 0.075)) {
    cat <- 3
  } else if ((PfPR >= 0.4) & (au5mr_cat < 0.06)) {
    cat <- 3
  } else if ((PfPR >= 0.1 &
              PfPR <= 0.2) &
             (au5mr_cat >= 0.06 & au5mr_cat < 0.075)) {
    cat <- 4
  } else if ((PfPR >= 0.2 & PfPR <= 0.4) & (au5mr_cat < 0.06)) {
    cat <- 4
  } else if ((PfPR >= 0.1 & PfPR <= 0.2) & (au5mr_cat < 0.06)) {
    cat <- 5
  }
  return(as.character(cat))
}
### adm2 -----
sle_malvec_cat_adm2 <-
  sle_pfpr_adm2_2019$data |>
  left_join(ihme_all_mortality_u5_sle_adm2$data, by = c('adm2')) |>
  select(adm2, PfPR, au5mr_cat) |>
  rowwise() |>
  mutate(category = as.factor(categorization(PfPR, au5mr_cat)))

write_csv(sle_malvec_cat_adm2, "sle_malvec_cat_adm2.csv")

sle_malvec_cat_adm2_map_data <-
  sle_adm2_map |> left_join(sle_malvec_cat_adm2, by = c("New_Dist" = "adm2"))
sle_malvec_cat_adm2_map <- tm_shape(sle_malvec_cat_adm2_map_data) +
  tm_polygons("category",
              palette = c("#761a7b", "#ee3d8d", "#ef87be", "#4eac32", "#95d689"))
print(sle_malvec_cat_adm2_map)
tmap_save(sle_malvec_cat_adm2_map, "sle_malvec_cat_adm2.png")


### adm3 -----
sle_malvec_cat_adm3 <-
  sle_pfpr_adm3_2019$data |>
  left_join(ihme_all_mortality_u5_sle_adm3$data, by = c('adm2', 'adm3')) |>
  select(adm2, adm3, PfPR, au5mr_cat) |>
  rowwise() |>
  mutate(category = categorization(PfPR, au5mr_cat))
write_csv(sle_malvec_cat_adm3, "sle_malvec_cat_adm3.csv")

sle_malvec_cat_adm3_map_data <-
  sle_adm3_map |> left_join(sle_malvec_cat_adm3, by = c("New_Chief" = "adm3"))
sle_malvec_cat_adm3_map <- tm_shape(sle_malvec_cat_adm3_map_data) +
  tm_polygons("category",
              palette = c("#761a7b", "#ee3d8d", "#ef87be", "#4eac32", "#95d689"))
print(sle_malvec_cat_adm3_map)
tmap_save(sle_malvec_cat_adm3_map, "sle_malvec_cat_adm3.png")

## Population map
pop_data_adm2  <-
  routine_data |> filter(year == 2021) |> select(adm2, pop2022) |> distinct()

pop_map <-
  sle_adm2_map |> left_join(pop_data_adm2, by = c("New_Dist" = "adm2"))
pop_map <-
  tm_shape(pop_map) +
  tm_polygons("pop2022") +
  tm_shape(sle_adm3_map) +
  tm_borders(col = "grey")

tmap_save(pop_map, filename = "SLE_pop_map.png")
