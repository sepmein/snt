# Import Package
library(snt)
library(tidyverse)
library(readxl)
library(heaven)

set_country(root_folder = "/Users/sepmein/dev/working/snt-data",
            country = "BDI")

#######################
# Import Routine Data #
#######################

### suspect cases
susp <- smart_read_excel(
  "Countries/BDI/2020_SNT/Analysis/orig/data/routine/monthly/CAS_SUSPECTES_PALU_EN_*yyyy*.xls",
  skip = 2,
  clean = TRUE,
  country = "BDI"
)
susp <- susp |>
  mutate(data = map(data, ~ rename_with(.x, ~ str_remove(.x, " [12][0-9]{3}$")))) |>
  # combine all susceptible data
  unnest(data) |> # reshape data
  pivot_longer(cols = 7:18,
               names_to = "month",
               values_to = "susp")

### tested and confirmed
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

### mal treat under 5
maltreat_u5 <- smart_read_excel(
  "countries/bdi/2020_snt/analysis/orig/data/routine/monthly/cas_traites_act_de_moins_de_5_ans_en_*yyyy*.xls",
  skip = 2
)
maltreat_u5 <- maltreat_u5 |>
  unnest(data) |>
  pivot_longer(cols = 7:18,
               names_to = "month",
               values_to = "maltreat_u5")

### mal treat over 5
maltreat_ov5 <- smart_read_excel(
  "Countries/BDI/2020_SNT/Analysis/orig/data/routine/monthly/CAS_TRAITES_ACT_5_ANS_ET_PLUS_*yyyy*.xls"
)
maltreat_ov5 <- maltreat_ov5 |>
  unnest(data) |>
  pivot_longer(cols = 7:18,
               names_to = "month",
               values_to = "maltreat_ov5")

### malaria death
maldth <- smart_read_excel(
  "Countries/BDI/2020_SNT/Analysis/orig/data/routine/monthly/Decès_lié_au_palu_*yyyy*.xls"
)

# 2016 data structure is different from others
# deal with 2016 first
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

### merge
routine_monthly <- full_join(susp, testconf)
routine_monthly <- full_join(routine_monthly, maltreat_u5)
routine_monthly <- full_join(routine_monthly, maltreat_ov5)
routine_monthly <- full_join(routine_monthly, maldth)

routine_monthly <- routine_monthly |>
  select(-Pays) |>
  # drop if Districts==""
  # drops a HF called Gitega in Gitega province,
  # but without district info and only reported
  # in the testconf database in 2016 and 2019
  filter(adm2_ds != "") |>
  mutate(adm2 = str_remove(adm2_ds, "DS\\s")) |>
  select(-adm2_ds) |>
  mutate(hf = paste(adm1, adm2, hfname, sep = "-")) |>
  mutate(yearmon = str_c(year, str_pad(month, 2, pad = "0"))) |>
  rowid_to_column("ID")

routine_monthly <- routine_monthly |>
  mutate(test = test_rdt + test_mic) |>
  mutate(maltreat = maltreat_u5 + maltreat_ov5) |>
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

# Get Confirmed RDT
conf_rdt <- routine_monthly |>
  arrange(yearmon) |>
  left_join(import_routine_set_cluster) |>
  group_by(yearmon, cluster) |>
  summarise(conf_rdt = sum(conf_rdt, na.rm = TRUE))

### plot by cluster
ggplot(data = conf_rdt, aes(x = yearmon, y = conf_rdt, group = cluster)) +
  geom_line() +
  scale_x_discrete(guide = guide_axis(check.overlap = TRUE)) +
  labs(title = "Confirmed RDT tests by cluster", y = "Confirmed RDT Test") +
  facet_wrap( ~ cluster, scales = "free")

##################
### Outlier detect
##################

### plot

### test
outlier_test <- EnvStats::rosnerTest(routine_monthly$maldth, 10000)
outlier_test$all.stats

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


out <- boxplot.stats(routine_monthly$susp)$out
out_ind <- which(routine_monthly$susp %in% c(out))
out_ind
routine_monthly[out_ind, ]

EnvStats::rosnerTest(routine_monthly$maldth,
                     k = 20, alpha = 0.00001)




outliers <- find_outiler(routine_monthly)


outliers_find_hf(outliers)
