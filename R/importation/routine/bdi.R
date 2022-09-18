# Import Package
library(snt)
library(tidyverse)
library(readxl)
# Importation Routine Data For BDI
set_country(root = "/Users/sepmein/dev/working/snt-data", 
country="BDI")
suspect_2016  <- read_excel(
                            "/Users/sepmein/dev/working/snt-data/Countries/BDI/2020_SNT/Analysis/orig/data/routine/monthly/CAS_SUSPECTES_PALU_EN_2016.xls",
skip=2)

suspect_2016 %>%
  rename(Country = Pays) %>%

