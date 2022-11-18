# Load package -----
setwd("../snt")
devtools::load_all()
library(snt)
library(tidyverse)
library(readxl)
library(haven)
library(tmap)
library(sf)

# Meta ------
## Path ------
set_country(
    root_folder = file.path("/Users/sepmein/Library/CloudStorage/OneDrive-SharedLibraries-WorldHealthOrganization/GMP-SIR\ -\ Nigeria"),
    country = "NGA"
)

incid_2014_2020_path <- "Data/Clean/incid_2014-2020.dta"
incid_2021_path <- "Data/Clean/incid_2021.dta"
smc_before_2020 <- "Data/NGA_Intervention_data_29-01-2020_CORRECTED_bg.xlsx"
smc_after_2020 <- "Data/Clean/nigeria_smc_2021.xlsx"
llins_before_2020 <- smc_before_2020
llins_after_2020 <- "Data/Clean/nga_adm1_llins_2020_2021.xlsx"
ento_file <- "Data/"

## Shapefiles -----
nga_adm1_shapefile <-
    "NGA_clean_Shp/NGA_States_clean.shp"
nga_adm1_map <- st_read(nga_adm1_shapefile)

## Matching Adm1 name with Shapefiles -----
### Step1: export names in Shape file into a csv file -----
# nga_adm1 <- nga_adm1_map |> select(State)
# write_csv(nga_adm1, "nga_adm1.csv")

### Step2: manually compare the adm1 with the one in the country data ------


# 1. Import data -----

## 1.1 Bea incid-----

### The data was stored in two dta file for Nigeria
### The data structure is different from each other
### So cleaning was required.

### Read files ------
#### incid 2014 - 2022 -----
incid_2014_2020 <- read_dta(incid_2014_2020_path)
#### incid 2021 ----
incid_2021 <- read_dta(incid_2021_path) |>
    rename(adm1 = state) |>
    rename(pop = allagespop) |>
    rename(pop_u5 = u5pop) |>
    rename(adm2 = LGAName)

### Merge -----
nga_adm1 <- bind_rows(incid_2014_2020, incid_2021)

### Clean -----
nga_adm1 <- nga_adm1 |>
    mutate_at(
        c("adm1"),
        ~ str_replace(., "Akwa-Ibom", "Akwa lbom")
    ) |>
    mutate_at(
        c("adm1"),
        ~ str_replace(., "Cross-River", "Cross River")
    ) |>
    mutate_at(
        c("adm1"),
        ~ str_replace(., "FCT-Abuja", "Federal Capital Territory")
    ) |>
    mutate_at(
        c("adm1"),
        ~ str_remove(., " State")
    ) |>
    mutate_at(
        c("adm1"),
        ~ str_remove(., " state")
    ) |>
    mutate_at(
        c("adm1"),
        ~ str_remove(., "^\\w{2} ")
    ) |>
    # mutate(year = as.factor(year)) |>
    filter(!(is.na(adm1)))

## DHS -------
### Read files -----
p_dhs_pfpr <- "Data/Dirty/DHS/malaria prevalence in children.xlsx"
p_dhs_intervention_distribution <- "Data/Dirty/DHS/itn_act_rdt_distributions.xlsx"
p_itn_usage_all <- "Data/Dirty/DHS/Existing insecticide-treated mosquito nets (ITNs) used last night.xlsx"
p_children_f_blood <- "Data/Dirty/DHS/Children with fever who had blood taken from a finger or heel for testing.xlsx"
p_itn_usage_u5 <- "Data/Dirty/DHS/Children under 5 who slept under any net.xlsx"
p_fever_treatment_u5 <- "Data/Dirty/DHS/Advice or treatment for fever sought from a health facility or provider.xlsx"
p_itn_usage <- "Data/Dirty/DHS/itn_usage.xlsx"

malaria_prevalence <-
    read_xlsx(p_dhs_pfpr, skip = 3)
itn_act_rdt_distributions <-
    read_xlsx(p_dhs_intervention_distribution, skip = 3)
itn_usage_all <-
    read_xlsx(p_itn_usage_all, skip = 3)
children_f_blood <-
    read_xlsx(p_children_f_blood, skip = 3)
itn_usage_u5 <-
    read_xlsx(p_itn_usage_all, skip = 3)
fever_treatment_u5 <-
    read_xlsx(p_fever_treatment_u5, skip = 3)
itn_usage <-
    read_xlsx(p_itn_usage, skip = 3)

### Merge -----
nga_dhs <- malaria_prevalence |>
    full_join(itn_act_rdt_distributions) |>
    full_join(itn_usage_all) |>
    full_join(children_f_blood) |>
    full_join(itn_usage_u5) |>
    full_join(fever_treatment_u5) |>
    full_join(itn_usage)

### Clean -----
nga_dhs <- nga_dhs |>
    mutate(Characteristic = str_remove(Characteristic, "Region : ")) |>
    mutate(Characteristic = str_remove(Characteristic, "\\)")) |>
    filter(Characteristic != "Total") |>
    separate(Characteristic,
        sep = "\\(",
        into = c("State", "Level")
    ) |>
    mutate_at("State", str_trim) |>
    mutate_at(
        "State",
        ~ str_replace(.x, "FCT Abuja", "Federal Capital Territory")
    ) |>
    mutate_at("State", ~ str_replace(.x, "Akwa Ibom", "Akwa lbom")) |>
    separate(Survey, sep = " ", into = c("Year", "Survey")) |>
    mutate_at(
        c(
            "Malaria prevalence according to RDT",
            "Malaria prevalence according to microscopy"
        ),
        ~ as.numeric(str_extract(., "^\\d{2}.\\d"))
    ) |>
    mutate(Year = as.factor(Year)) |>
    write_csv("nigeria_dhs-1.csv")

## MIS --------
### Read files ------
household_possession_nets <- read_excel(
    "Data/Dirty/MIS/Table 3.1.2 Household possession of mosquito nets- States.xlsx"
)

access_to_itn <- read_excel(
    "Data/Dirty/MIS/Table 3.3.2 Access to an insecticide-treated net (ITN)- States.xlsx"
) |>
    rename(acc_itn = "Percentage of the de facto population with access to an ITN") |>
    select(State, acc_itn)

use_of_itn_in_household <- read_excel(
    "Data/Dirty/MIS/Table 3.4.2 Use of mosquito nets by persons in the household- States.xlsx"
)

use_of_itn <- read_excel(
    "Data/Dirty/MIS/Table 3.5.2 Use of existing ITNs- States.xlsx"
)

use_of_itn_children <- read_excel(
    "Data/Dirty/MIS/Table 3.6.2 Use of mosquito nets by children- States.xlsx"
)

treatment_seeking <- read_excel(
    "Data/Dirty/MIS/Table 4.1.2 Children with fever and care seeking, prompt treatment, and diagnosis- States.xlsx"
)

prevalence_in_children <- read_excel(
    "Data/Dirty/MIS/Table 4.8.2 Prevalence of malaria in children- States.xlsx"
) |>
    rename(
        prev_rdt = "Malaria prevalence according to RDT - RDT positive",
        prev_mic = "Malaria prevalence according to RDT - Microscopy positive"
    ) |>
    select(State, prev_rdt, prev_mic)

### Merge ------
mis <- household_possession_nets |>
    full_join(access_to_itn) |>
    full_join(use_of_itn_in_household) |>
    full_join(use_of_itn) |>
    full_join(use_of_itn_children) |>
    full_join(treatment_seeking) |>
    full_join(prevalence_in_children)
mis <- mis |>
    rename(adm1 = State) |>
    mutate_at(c("adm1"), ~ str_replace(., "Akwa-Ibom", "Akwa lbom")) |>
    mutate_at(c("adm1"), ~ str_replace(., "Cross-River", "Cross River")) |>
    mutate_at(c("adm1"), ~ str_replace(., "FCT-Abuja", "Federal Capital Territory")) |>
    mutate(year = 2021)

## Combine DHS and MIS data -----

# Combine survey data and routine data ------

# export ------

nga_dhs <- read_csv("Data/Clean/nigeria_dhs.csv")

nga_adm1 |>
    select(year, adm1, pop, pop_u5)

# import into database ------

## population ------
