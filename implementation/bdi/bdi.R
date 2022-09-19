# Import Package
library(snt)
library(tidyverse)
library(purrr)
library(readxl)

set_country(
  root = "/Users/sepmein/dev/working/snt-data",
  country = "BDI"
)

#######################
# Import Routine Data #
#######################

suspects <- smart_read_excel(
  smart_path = "Countries/BDI/2020_SNT/Analysis/orig/data/routine/monthly/CAS_SUSPECTES_PALU_EN_*yyyy*.xls",
  skip = 2
)
suspects %>%
  pivot_longer(.x,
    cols = 6:17, names_to = "month",
    values_to = "suspect"
  ) %>%
  map(~ routine_rename(.x, country = "BDI")) %>%
  map(~ routine_replace(.x, country = "BDI")) %>%
  map(~ mutate_at(.x, ))
