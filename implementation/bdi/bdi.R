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
  "Countries/BDI/2020_SNT/Analysis/orig/data/routine/monthly/CAS_SUSPECTES_PALU_EN_*yyyy*.xls",
  skip = 2
)
a <- suspects %>%
  # rename using internal rename database
  mutate(data = map(
    data,
    ~ routine_rename(.x, country = "BDI")
  )) %>%
  # remove space + year, from "2 2018" to "2"
  mutate(data = map(
    data,
    ~ rename_with(.x, ~ str_remove(.x, " [12][0-9]{3}$"))
  )) %>%
  # combine all susceptible data
  unnest(data) %>%
  # reshape data
  pivot_longer(
    cols = 7:18, names_to = "month",
    values_to = "suspect"
  )
