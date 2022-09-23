# Import Package
library(snt)
library(tidyverse)
library(readxl)
library(haven)
library(tmap)

set_country(root_folder = "/Users/sepmein/dev/working/snt-data",
            country = "BDI")

# 1. Importation ---------------------------------------------------------------
## 1.1 Suspect -------------------------------------------------------------
susp <- smart_read_excel(
  "Countries/BDI/2020_SNT/Analysis/orig/data/routine/monthly/CAS_SUSPECTES_PALU_EN_*yyyy*.xls",
  skip = 2,
  clean = TRUE,
  country = "BDI"
)
susp <- susp |>
  # rename row title from
  mutate(data = map(data, ~ rename_with(.x, ~ str_remove(.x, " [12][0-9]{3}$")))) |>
  # combine all susceptible data
  unnest(data) |> # reshape data
  pivot_longer(cols = 7:18,
               names_to = "month",
               values_to = "susp")

## 1.2 tested and confirmed -----------------------------------------------
testconf <- smart_read_excel(
  "Countries/BDI/2020_SNT/Analysis/orig/data/routine/monthly/TDR_et_GE_*yyyy*.xls"
)
testconf <- testconf |>
  unnest(data) |>
  pivot_longer(cols = 7:78,
               names_to = "index",
               values_to = "testconf") |>
  separate(index, sep = "_(?!.*_)", into = c("index", "month")) |>
  pivot_wider(names_from = "index", values_from = "testconf")

## 1.3 Malaria Treatment Under 5 -------------------------------------------
maltreat_u5 <- smart_read_excel(
  "countries/bdi/2020_snt/analysis/orig/data/routine/monthly/cas_traites_act_de_moins_de_5_ans_en_*yyyy*.xls",
  skip = 2
)
maltreat_u5 <- maltreat_u5 |>
  unnest(data) |>
  pivot_longer(cols = 7:18,
               names_to = "month",
               values_to = "maltreat_u5")

## 1.4 Malaria Treatment Over 5 --------------------------------------------
maltreat_ov5 <- smart_read_excel(
  "Countries/BDI/2020_SNT/Analysis/orig/data/routine/monthly/CAS_TRAITES_ACT_5_ANS_ET_PLUS_*yyyy*.xls"
)
maltreat_ov5 <- maltreat_ov5 |>
  unnest(data) |>
  pivot_longer(cols = 7:18,
               names_to = "month",
               values_to = "maltreat_ov5")

## 1.5 Malaria Death -------------------------------------------------------
maldth <- smart_read_excel(
  "Countries/BDI/2020_SNT/Analysis/orig/data/routine/monthly/Decès_lié_au_palu_*yyyy*.xls"
)
# 2016 data structure is different from others deal with 2016 first
maldth$data[[1]] <- maldth$data[[1]] |>
  pivot_longer(cols = 8:103,
               names_to = "index",
               values_to = "value") |>
  separate(index, sep = "_(?!.*_)", into = c("index", "month")) |>
  pivot_wider(names_from = "index", values_from = "value") |>
  rowwise() |>
  mutate(maldth = sum(c_across(maldth_011m:maldth_ov50), na.rm = TRUE)) |>
  select(-contains("maldth_")) |>
  mutate(index = paste0("maldth_", month)) |>
  select(-c("month")) |>
  pivot_wider(names_from = index, values_from = "maldth")

maldth <- maldth |>
  unnest(data) |>
  pivot_longer(cols = 9:20,
               names_to = "index",
               values_to = "value") |>
  separate(index, sep = "_(?!.*_)", into = c("index", "month")) |>
  pivot_wider(names_from = "index", values_from = "value") |>
  select(-Centre)

### merge all four databases into one
routine_monthly <- full_join(susp, testconf)
routine_monthly <- full_join(routine_monthly, maltreat_u5)
routine_monthly <- full_join(routine_monthly, maltreat_ov5)
routine_monthly <- full_join(routine_monthly, maldth)

### Continue data cleaning and transformation
routine_monthly <- routine_monthly |>
  select(-Pays) |>
  # drop if Districts==""
  # drops a HF called Gitega in Gitega province,
  # but without district info and only reported
  # in the testconf database in 2016 and 2019
  filter(adm2_ds != "") |>
  # change from "DS gerate" to "gerate"
  mutate(adm2 = str_remove(adm2_ds, "DS\\s")) |>
  # remove adm2_ds column
  select(-adm2_ds) |>
  # create a new column named hf by combine adm1, adm2 & hfname
  mutate(hf = paste(adm1, adm2, hfname, sep = "-")) |>
  # create yearmon column by year and month
  mutate(yearmon = str_c(year, str_pad(month, 2, pad = "0"))) |>
  # create a new ID column by row number
  rowid_to_column("ID")

routine_monthly <- routine_monthly |>
  # compute test and malaria treatment
  mutate(test = test_rdt + test_mic) |>
  mutate(maltreat = maltreat_u5 + maltreat_ov5) |>
  # select and order the columns
  select(
    ID,
    adm1,
    adm2,
    adm3,
    hfca,
    hfname,
    hf,
    year,
    month,
    yearmon,
    susp,
    test_rdt,
    test_mic,
    test_rdt_lab,
    test,
    abn_mic,
    abn_rdt,
    conf_rdt,
    maltreat_u5,
    maltreat_ov5,
    maltreat,
    maldth
  )

# get location info by reading and merging from hfs_2013.dta
hfs <-
  haven::read_dta("Countries/BDI/2020_SNT/Analysis/dta/pri/hfs_2013.dta") |>
  select(Latitude, Longitude, hfname, adm1, adm2, hfid, hf)

routine_monthly <- routine_monthly |>
  left_join(hfs)


# 2. Analyze -----------------------------------------------------------------
## 2.1 Summaries ---------------------------------------------------------------
### 2.1.1 N/A values --------------------------------------------------------------
View(routine_monthly |>
       na_if("") |>
       map_df( ~ sum(is.na(.))))

### 2.2.2 Summaries ---------------------------------------------------------
routine_monthly |> select_if(is_numeric) |> skimr::skim()

## 2.2 Confirmed RDTs ------------------------------------------------------
conf_rdt <- routine_monthly |>
  arrange(yearmon) |>
  left_join(import_routine_set_cluster) |>
  group_by(yearmon, cluster) |>
  summarise(conf_rdt = sum(conf_rdt, na.rm = TRUE))

# plot by cluster
ggplot(data = conf_rdt, aes(x = yearmon, y = conf_rdt, group = cluster)) +
  geom_line() +
  scale_x_discrete(guide = guide_axis(check.overlap = TRUE)) +
  labs(title = "Confirmed RDT tests by cluster", y = "Confirmed RDT Test") +
  facet_wrap(~ cluster, scales = "free")

## 2.3 Outliers ------------------------------------------------------------

### Plot --------------------------------------------------------------------
routine_monthly |>
  select(
    year,
    susp,
    test_rdt,
    test_mic,
    test_rdt_lab,
    test,
    abn_mic,
    abn_rdt,
    conf_rdt,
    maltreat_u5,
    maltreat_ov5,
    maltreat,
    maldth
  ) |>
  pivot_longer(cols = 2:13,
               names_to = "index",
               values_to = "value") |>
  # mutate(year= factor(year)) |>
  # ggplot(aes(x = value, y = index)) +
  # geom_boxplot() +
  # geom_point(aes(shape = year),alpha = 0.01)
  ggstatsplot::ggbetweenstats(x = index,
                              y = value,
                              pairwise.comparisons = FALSE)

### List --------------------------------------------------------------------
outliers <- find_outiler(routine_monthly)
outliers_by_hf <- outliers_find_hf(outliers)

# outlier_test <- EnvStats::rosnerTest(routine_monthly$maldth, 10000)
# outlier_test$all.stats

## 2.4 Reporting Rates ----------------------------------

### Plot by HF & Date -------------------------------------------------------
report_status_by_hf_and_date <- routine_monthly |>
  group_by(hf, yearmon) |>
  summarise(sum = sum(c_across(susp:maldth), na.rm = TRUE)) |>
  mutate(reported = if_else(sum > 0, "Y", "N"))
report_status_by_hf_and_date |>
  ggplot(aes(x = yearmon, y = hf, fill = reported)) +
  geom_tile() +
  scale_x_discrete(guide = guide_axis(check.overlap = TRUE)) +
  labs(title = "Report Status By Health Facilitis and Date",
       y = "Health Facilities",
       x = "Date") +
  theme(axis.text.y = element_blank()) +
  scale_fill_manual(values = c("N" = "white", "Y" = "blue"))

### Report Rate Table -------------------------------------------------------
report_rate_by_hf <- report_status_by_hf_and_date |>
  mutate(reported_value = ifelse(reported == "Y", 1, 0)) |>
  group_by(hf) |>
  summarise(mean_report_rate = sum(reported_value) / n()) |>
  arrange(mean_report_rate)
View(report_rate_by_hf)

### Plot Map ----------------------------------------------------------------
report_rate_by_hf_coords <-
  report_rate_by_hf |> inner_join(hfs) |>
  mutate(Latitude = str_replace(Latitude, ",", ".")) |>
  mutate(Longitude = str_replace(Longitude, ",", ".")) |>
  st_as_sf(coords = c("Longitude", "Latitude"))

tmap::tm_shape(report_rate_by_hf_coords) +
  tmap::tm_dots(col = "mean_report_rate", size = 0.01, alpha = 0.5)


### Plot Indicators & Date --------------------------------------------------
report_status_by_indicators_and_date <- routine_monthly |>
  pivot_longer(
    cols = c("test", "susp", "maltreat", "maldth", "conf_rdt"),
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

### Plot By ADM2 and Date ---------------------------------------------------
report_status_by_adm2_and_date <- routine_monthly |>
  pivot_longer(
    cols = susp:maldth,
    names_to = "index",
    values_to = "value"
  ) |>
  mutate(reported = if_else(value > 0, 1, 0)) |>
  group_by(adm2, yearmon) |>
  summarise(reports = sum(reported, na.rm = TRUE), n = n()) |>
  mutate(report_rate = reports / n)

ggplot(report_status_by_adm2_and_date) +
  geom_tile(aes(x = yearmon, y = adm2, fill = report_rate)) +
  scale_fill_viridis_c()+
  scale_x_discrete(guide = guide_axis(check.overlap = TRUE)) +
  labs(title = "Report Status By Adm2 and Date",
       y = "Districts",
       x = "Date")

# TODO generate a list and export

## 2.4 Consistency test --------------------------------------------------------
