# load data ------
wmr <- read_csv("Data/00_incidence_rate_all_age_table_Nigeria_admin1_2000-2023.csv") |>
  mutate(incidence_rate_all_age_rmean = incidence_rate_all_age_rmean * 1000) |>
  mutate_at(c("Name"), ~ str_replace(., "Akwa Ibom", "Akwa lbom")) |>
  mutate_at(c("Name"), ~ str_replace(., "Nassarawa", "Nasarawa")) |>
  mutate_at(c("Name"), ~ str_replace(., "Abuja", "Federal Capital Territory")) |>
  rename(incidence = incidence_rate_all_age_rmean)

wmr_2021 <- read_csv("Data/00_incidence_rate_all_age_table_Nigeria_admin1_2000-2023.csv") |>
  filter(Year %in% c(2014, 2018, 2021)) |>
  mutate(incidence_rate_all_age_rmean = incidence_rate_all_age_rmean * 1000) |>
  mutate_at(c("Name"), ~ str_replace(., "Akwa Ibom", "Akwa lbom")) |>
  mutate_at(c("Name"), ~ str_replace(., "Nassarawa", "Nasarawa")) |>
  mutate_at(c("Name"), ~ str_replace(., "Abuja", "Federal Capital Territory"))


## Map and data -----
nga_dhs_adm1 <-
  nga_dhs |>
  filter(Level == "L2")

nga_dhs_adm1_map_and_data <-
  nga_adm1_map |>
  left_join(nga_dhs_adm1,
    by = "State"
  ) |>
  mutate(Year = as.numeric(Year))

wmr_2021_map_and_data <- nga_adm1_map |>
  left_join(wmr_2021, by = c("State" = "Name"))

# Calculation -----
### Population -------
pop <- nga_adm1 |>
  select(year, adm1, pop, pop_u5) |>
  group_by(year, adm1) |>
  summarise(
    pop = sum(pop, na.rm = TRUE),
    pop_u5 = sum(pop_u5, na.rm = TRUE)
  ) |>
  arrange(adm1)

# write_csv(pop, "nigeria_pop.csv")

#### Country - Map --------
pop_map_and_data <- nga_adm1_map |>
  left_join(pop, by = c("State" = "adm1"))

pop_three_years <- pop_map_and_data |>
  filter(year %in% c(2014, 2018, 2021))

pop_map <-
  tm_shape(pop_three_years) +
  tm_polygons("pop") +
  tm_facets(by = "year")
print(pop_map)
tmap_save(pop_map, "Nigeria - country - population - map.png")

#### District - Trends------
ggplot(pop, aes(y = pop, x = as.factor(year))) +
  geom_bar(stat = "identity") +
  scale_x_discrete(guide = guide_axis(check.overlap = TRUE)) +
  labs(title = "Trend of Population", y = "Population", x = "Year") +
  facet_wrap(~adm1)
ggsave("Nigeria - states - population - barplot.png")

# Prevalence DHS or MAP
# [Monday 23:58] GALATAS ANDRADE, Beatriz
# Prevalence: There are various ways you can do it:
# Download the annual rasters between 2010 and 2020 from MAP's website and extract the mean to the adm1 shapefiles MAP has done a specific analysis for Nigeria only for 2018-2021 that I have placed in the file "output_nga_20181921.csv" focus on the variable "PFPR" and you can do the mean of that

## Prevalence -----
### RDT -----
#### Country -----
nga_adm1_prevalence_rdt_map <-
  nga_dhs_adm1_map_and_data |>
  filter(Year != 2013) |>
  tm_shape() +
  tm_polygons("Malaria prevalence according to RDT") +
  tm_facets(by = "Year")
tmap_save(
  nga_adm1_prevalence_rdt_map,
  "Nigeria - country - Malaria prevalence according to RDT - map.png"
)

#### Barplot facet ADM1 ------
nga_dhs_adm1 |>
  filter(Year != 2013) |>
  ggplot() +
  geom_bar(
    aes(
      x = Year,
      y = `Malaria prevalence according to RDT`
    ),
    stat = "identity"
  ) +
  scale_x_discrete(guide = guide_axis(check.overlap = TRUE)) +
  labs(y = "Malaria prevalence according to RDT", x = "Year") +
  facet_wrap(~State)

ggsave("Nigeria - States - Malaria prevalence according to RDT - Barplot.png")

### Microscopy -----
#### Country ------
nga_adm1_prevalence_mic_map <-
  nga_dhs_adm1_map_and_data |>
  filter(Year != 2013) |>
  tm_shape() +
  tm_polygons("Malaria prevalence according to microscopy") +
  tm_facets(by = "Year")
tmap_save(
  nga_adm1_prevalence_mic_map,
  "Nigeria - country - Malaria prevalence according to microscopy - map.png"
)

#### Barplot facet ADM1 ------
nga_dhs_adm1 |>
  filter(Year != 2013) |>
  ggplot() +
  geom_bar(
    aes(
      x = Year,
      y = `Malaria prevalence according to microscopy`
    ),
    stat = "identity"
  ) +
  scale_x_discrete(guide = guide_axis(check.overlap = TRUE)) +
  labs(y = "Malaria prevalence according to microscopy", x = "Year") +
  facet_wrap(~State)

ggsave("Nigeria - States - Malaria prevalence according to microscopy - Barplot.png")


## Cases ------
# [Monday 23:59] GALATAS ANDRADE, Beatriz
# Cases: You can find them in the folder incid_2014-2020 and in NGA_routinedata_district_year_2021 (look for "conf" variable)

### Calculate -----
nga_adm1_conf <- nga_adm1 |>
  filter(year %in% c(2014, 2018, 2021)) |>
  group_by(adm1, year) |>
  summarise(conf = sum(conf, na.rm = TRUE))

### Country -----
nga_adm1_conf_map <-
  nga_adm1_map |>
  left_join(nga_adm1_conf, by = c("State" = "adm1")) |>
  tm_shape() +
  tm_polygons("conf") +
  tm_facets(by = "year")
tmap_save(
  nga_adm1_conf_map,
  "Nigeria - country - confirmed cases - map.png"
)

### Barplot facet by adm1 -----
nga_adm1 |>
  group_by(adm1, year) |>
  summarise(conf = sum(conf, na.rm = TRUE)) |>
  ggplot() +
  geom_bar(
    aes(
      x = year,
      y = conf
    ),
    stat = "identity"
  ) +
  scale_x_discrete(guide = guide_axis(check.overlap = TRUE)) +
  labs(y = "Confirmed Malaria Cases", x = "year") +
  facet_wrap(~adm1)

ggsave("Nigeria - States - confirmed cases - Barplot.png")

## Incidences ------
# [Monday 23:59] GALATAS ANDRADE, Beatriz
# Iincidence: Same as for cases

### Calc -------
nga_adm1_inci <- nga_adm1 |>
  filter(year %in% c(2014, 2018, 2021)) |>
  mutate(adjinc3_pop = adjinc3 * pop) |>
  group_by(adm1, year) |>
  summarise(
    adjinc3_pop = sum(adjinc3_pop, na.rm = TRUE),
    pop = sum(pop, na.rm = TRUE)
  ) |>
  mutate(
    incidence = adjinc3_pop / pop
  )

### Country Map ------
nga_adm1_inci_map <-
  nga_adm1_map |>
  left_join(nga_adm1_inci, by = c("State" = "adm1")) |>
  tm_shape() +
  tm_polygons("incidence") +
  tm_facets(by = "year")
tmap_save(
  nga_adm1_inci_map,
  "Nigeria - country - incidence per thousand - map.png"
)

### Barplot facet adm1------
nga_adm1_inci <- nga_adm1 |>
  group_by(adm1, year) |>
  summarise(
    conf = sum(conf, na.rm = TRUE),
    pop <- sum(pop, na.rm = TRUE)
  ) |>
  mutate(
    incidence = conf / pop * 1000
  ) |>
  ggplot() +
  geom_bar(
    aes(
      x = year,
      y = incidence
    ),
    stat = "identity"
  ) +
  scale_x_discrete(guide = guide_axis(check.overlap = TRUE)) +
  labs(y = "Malaria Incidence(per thousand)", x = "year") +
  facet_wrap(~adm1)

ggsave("Nigeria - States - Malaria Incidence(per thousand - Barplot.png")

# [Yesterday 00:00] GALATAS ANDRADE, Beatriz
# All others, you can extract them from Stat compiler at the regional level (in Nigeria, one region covers many adm1s). I dont think we can get lower level estimates without a small area estimation analysis

# ITN/ACT/RDT distribution

nga_dhs_adm1 |>
  ggplot() +
  geom_bar(
    aes(
      x = Year,
      y = `Population who slept under an insecticide-treated mosquito net (ITN) last night`
    ),
    stat = "identity"
  ) +
  scale_x_discrete(guide = guide_axis(check.overlap = TRUE)) +
  labs(y = "Population who slept under an insecticide-treated mosquito net (ITN) last night", x = "Year") +
  facet_wrap(~State)

ggsave("Nigeria - States - Population who slept under an insecticide-treated mosquito net (ITN) last night - Barplot.png")

# ITN-usage u5
nga_adm1_itn_u5_map <-
  nga_dhs_adm1_map_and_data |>
  tm_shape() +
  tm_polygons("Children under 5 who slept under an insecticide-treated net (ITN)") +
  tm_facets(by = "Year")
tmap_save(
  nga_adm1_itn_u5_map,
  "Nigeria - country - Children under 5 who slept under an insecticide-treated net (ITN) - map.png"
)
# ITN-usage all
nga_adm1_itn_all_map <-
  nga_dhs_adm1_map_and_data |>
  tm_shape() +
  tm_polygons("Population who slept under an insecticide-treated mosquito net (ITN) last night") +
  tm_facets(by = "Year")
tmap_save(
  nga_adm1_itn_all_map,
  "Nigeria - country - Population who slept under an insecticide-treated mosquito net (ITN) last night - map.png"
)

# ITN-usage ov5
nga_adm1_itn_ov5_map <- nga_dhs_adm1_map_and_data |>
  left_join(pop, by = c("State" = "adm1", "Year" = "year")) |>
  mutate(
    n_all_itn_use = `Population who slept under an insecticide-treated mosquito net (ITN) last night` * pop,
    n_u5_itn_use = `Children under 5 who slept under an insecticide-treated net (ITN)` * pop_u5
  ) |>
  mutate(
    itn_ov5 = (n_all_itn_use - n_u5_itn_use) / (pop - pop_u5)
  ) |>
  filter(Year != 2013) |>
  tm_shape() +
  tm_polygons("itn_ov5") +
  tm_facets(by = "Year")
tmap_save(
  nga_adm1_itn_ov5_map,
  "Nigeria - country - ITN usage in Population ov5 - map.png"
)

# per state plot
nga_dhs_adm1 |>
  ggplot() +
  geom_bar(
    aes(
      x = Year,
      y = `Population who slept under an insecticide-treated mosquito net (ITN) last night`
    ),
    stat = "identity"
  ) +
  scale_x_discrete(guide = guide_axis(check.overlap = TRUE)) +
  labs(y = "Population who slept under an insecticide-treated mosquito net (ITN) last night", x = "Year") +
  facet_wrap(~State)

ggsave("Nigeria - States - Population who slept under an insecticide-treated mosquito net (ITN) last night - Barplot.png")

nga_dhs_adm1 |>
  ggplot() +
  geom_bar(
    aes(
      x = Year,
      y = `Children under 5 who slept under an insecticide-treated net (ITN)`
    ),
    stat = "identity"
  ) +
  scale_x_discrete(guide = guide_axis(check.overlap = TRUE)) +
  labs(y = "Children under 5 who slept under an insecticide-treated net (ITN)", x = "Year") +
  facet_wrap(~State)

ggsave("Nigeria - States - Children under 5 who slept under an insecticide-treated net (ITN) - Barplot.png")

nga_dhs_adm1_map_and_data |>
  left_join(pop, by = c("State" = "adm1", "Year" = "year")) |>
  mutate(
    n_all_itn_use = `Population who slept under an insecticide-treated mosquito net (ITN) last night` * pop,
    n_u5_itn_use = `Children under 5 who slept under an insecticide-treated net (ITN)` * pop_u5
  ) |>
  mutate(
    itn_ov5 = (n_all_itn_use - n_u5_itn_use) / (pop - pop_u5)
  ) |>
  filter(Year != 2013) |>
  ggplot() +
  geom_bar(
    aes(
      x = Year,
      y = `itn_ov5`
    ),
    stat = "identity"
  ) +
  scale_x_discrete(guide = guide_axis(check.overlap = TRUE)) +
  labs(y = "itn_ov5", x = "Year") +
  facet_wrap(~State)

ggsave("Nigeria - States - itn_ov5 - Barplot.png")

# Treatment for fever
nga_adm1_treatment_fever_map <-
  nga_dhs_adm1_map_and_data |>
  tm_shape() +
  tm_polygons("Advice or treatment for fever sought from a health facility or provider") +
  tm_facets(by = "Year")
tmap_save(
  nga_adm1_treatment_fever_map,
  "Nigeria - country - Advice or treatment for fever sought from a health facility or provider - map.png"
)

nga_dhs_adm1 |>
  ggplot() +
  geom_bar(
    aes(
      x = Year,
      y = `Advice or treatment for fever sought from a health facility or provider`
    ),
    stat = "identity"
  ) +
  scale_x_discrete(guide = guide_axis(check.overlap = TRUE)) +
  labs(y = "Advice or treatment for fever sought from a health facility or provider", x = "Year") +
  facet_wrap(~State)

ggsave("Nigeria - States - Advice or treatment for fever sought from a health facility or provider - Barplot.png")

# Percentage of fever finger/nillpic
nga_adm1_fever_blood_taken <-
  nga_dhs_adm1_map_and_data |>
  tm_shape() +
  tm_polygons("Children with fever who had blood taken from a finger or heel for testing") +
  tm_facets(by = "Year")
tmap_save(
  nga_adm1_fever_blood_taken,
  "Nigeria - country - Children with fever who had blood taken from a finger or heel for testing - map.png"
)

nga_dhs_adm1 |>
  ggplot() +
  geom_bar(
    aes(
      x = Year,
      y = `Children with fever who had blood taken from a finger or heel for testing`
    ),
    stat = "identity"
  ) +
  scale_x_discrete(guide = guide_axis(check.overlap = TRUE)) +
  labs(y = "Children with fever who had blood taken from a finger or heel for testing", x = "Year") +
  facet_wrap(~State)

ggsave("Nigeria - States - Children with fever who had blood taken from a finger or heel for testing - Barplot.png")

nga_adm1 <- nga_adm1 |>
  group_by(adm1, year) |>
  summarise(
    pop = sum(pop, na.rm = TRUE),
    conf = sum(conf, na.rm = TRUE),
    adjinc3 = sum(pop * adjinc3) / sum(pop)
  )

### one page for country

###
wmr_one_page <- function(nga_adm1, district) {
  if (is.na(district)) {
    fct <- nga_adm1 |>
      group_by(year)

    fct_survey <- nga_dhs_adm1
    district <- "nga"
  } else {
    fct <- nga_adm1 |>
      filter(
        adm1 == district
      ) |>
      group_by(year)

    fct_survey <- nga_dhs_adm1 |>
      filter(State == district)

    wmr_filtered <- wmr |>
      select(Name, Year, incidence, Pop) |>
      rename(adm1 = Name, year = Year, WMR = incidence) |>
      filter(
        adm1 == district,
        year > 2013
      ) |>
      group_by(year)
  }

  # plot pop
  fct |>
    select(year, pop) |>
    summarise(pop = sum(pop, na.rm = TRUE)) |>
    ggplot(aes(year, pop)) +
    geom_line() +
    geom_point() +
    labs(y = "Population") +
    theme(text = element_text(size = 5))
  ggsave(paste0(district, " - 1. pop.png"), height = 1.8, width = 3)

  # plot confirmed cases
  fct |>
    select(year, conf, pop) |>
    summarise(
      conf = sum(conf, na.rm = TRUE),
      pop = sum(pop, na.rm = TRUE)
    ) |>
    ggplot(
      aes(
        x = year,
        y = conf
      ),
      color = "red"
    ) +
    geom_line() +
    geom_point() +
    labs(y = "Confirmed Cases") +
    theme(text = element_text(size = 5))
  ggsave(paste0(district, " - 2. conf.png"), height = 1.2, width = 3)

  # incidence
  incid <- fct |>
    select(year, adjinc3, pop) |>
    mutate(adjinc3_pop = adjinc3 * pop) |>
    summarise(
      adjinc3_pop = sum(adjinc3_pop, na.rm = TRUE),
      pop = sum(pop, na.rm = TRUE)
    ) |>
    mutate(inci = adjinc3_pop / pop) |>
    rename(`Routine Data(Adjusted)` = inci)

  incid |>
    left_join(wmr_filtered) |>
    select(year, `Routine Data(Adjusted)`, WMR) |>
    pivot_longer(c(2:3)) |>
    ggplot(aes(
      x = year,
      y = value,
      color = name,
      group = name
    )) +
    geom_line() +
    geom_point() +
    labs(y = "Malaria Incidence / 1000") +
    theme(text = element_text(size = 5))
  ggsave(paste0(district, " - 3. incidence.png"), height = 1.2, width = 5)

  # WMR only incidence
  minimum_incidences <- min(wmr_filtered$WMR)
  min_incidences_y_lab <- minimum_incidences / 4
  wmr_filtered |>
    select(year, WMR) |>
    ggplot(aes(
      x = year,
      y = WMR
    )) +
    geom_line() +
    geom_point() +
    labs(y = "Estimated Incidence / 1000") +
    ylim(min_incidences_y_lab, NA) +
    theme(text = element_text(size = 5))
  ggsave(paste0(district, " - 3. WMR incidence.png"), height = 1.2, width = 3)

  # WMR only cases
  minimum_cases <- min(wmr_filtered$WMR * wmr_filtered$Pop / 1000)
  min_cases_y_lab <- minimum_cases / 4
  wmr_filtered |>
    select(year, WMR, Pop) |>
    mutate(cases = WMR * Pop / 1000) |>
    ggplot(aes(
      x = year,
      y = cases
    )) +
    geom_line() +
    geom_point() +
    labs(y = "Estimated Cases") +
    ylim(min_cases_y_lab, NA) +
    theme(text = element_text(size = 5))
  ggsave(paste0(district, " - 2. WMR cases.png"), height = 1.2, width = 3)

  fct_survey |>
    filter(Year != 2013) |>
    mutate(Year = as.factor(Year)) |>
    select(
      Year,
      "Malaria prevalence according to RDT",
      "Malaria prevalence according to microscopy"
    ) |>
    rename(prev_rdt = "Malaria prevalence according to RDT") |>
    rename(prev_mic = "Malaria prevalence according to microscopy") |>
    pivot_longer(
      cols = c("prev_rdt", "prev_mic"),
      names_to = "index",
      values_to = "value"
    ) |>
    ggplot(aes(x = Year)) +
    geom_bar(
      aes(fill = index, y = value),
      position = "dodge", group = "index",
      stat = "identity"
    ) +
    theme(
      legend.title = element_blank(),
      text = element_text(size = 5)
    ) +
    scale_fill_discrete(
      labels = c("Microscopy", "RDT")
    ) +
    labs(y = "Prevalence in Children Under 5")
  ggsave(paste0(district, " - 4. prev_rdt_mic.png"), height = 1.6, width = 3)


  fct_survey |>
    mutate(Year = as.factor(Year)) |>
    select(
      Year,
      "Population who slept under an insecticide-treated mosquito net (ITN) last night",
      "Children under 5 who slept under an insecticide-treated net (ITN)",
      "Children under 5 who slept under any net",
      "Existing insecticide-treated mosquito nets (ITNs) used last night"
    ) |>
    rename(itn_usage = "Population who slept under an insecticide-treated mosquito net (ITN) last night") |>
    rename(itn_usage_u5 = "Children under 5 who slept under an insecticide-treated net (ITN)") |>
    rename(anynet_usage_u5 = "Children under 5 who slept under any net") |>
    rename(exist_itn_usage = "Existing insecticide-treated mosquito nets (ITNs) used last night") |>
    pivot_longer(
      cols = c("itn_usage", "itn_usage_u5", "anynet_usage_u5", "exist_itn_usage"),
      names_to = "index",
      values_to = "value"
    ) |>
    ggplot(aes(x = Year)) +
    geom_bar(
      aes(fill = index, y = value),
      position = "dodge", group = "index",
      stat = "identity"
    ) +
    theme(
      legend.title = element_blank(),
      text = element_text(size = 5)
    ) +
    scale_fill_discrete(
      labels = c("% Anynet in Children Under 5", "% Existing ITN", "% ITN in All Age", "% ITN in Children Under 5")
    ) +
    labs(y = "% Usage")

  ggsave(paste0(district, " - 5. itn - usage.png"), height = 1.6, width = 3)

  fct_survey |>
    mutate(Year = as.factor(Year)) |>
    select(
      Year,
      "Advice or treatment for fever sought from a health facility or provider",
      "Children with fever who had blood taken from a finger or heel for testing"
    ) |>
    rename(Treatment_Seeking = "Advice or treatment for fever sought from a health facility or provider") |>
    rename(Fever_Blood_Taken_u5 = "Children with fever who had blood taken from a finger or heel for testing") |>
    pivot_longer(
      cols = c("Treatment_Seeking", "Fever_Blood_Taken_u5"),
      names_to = "index",
      values_to = "value"
    ) |>
    ggplot(aes(x = Year)) +
    geom_bar(
      aes(fill = index, y = value),
      position = "dodge", group = "index",
      stat = "identity"
    ) +
    theme(
      legend.title = element_blank(),
      text = element_text(size = 5)
    ) +
    scale_fill_discrete(
      labels = c("% Blood Taken", "% Treatment Sought")
    ) +
    labs(y = "% in Children had Fever in Last Two Weeks")
  ggsave(paste0(district, " - 6. fever.png"), height = 1.6, width = 3)

  ## add map for each country
  district_only_map <- nga_adm1_map |>
    mutate(target = if_else(State == district, TRUE, FALSE)) |>
    tm_shape() +
    tm_polygons("target",
      palette = c("#949393", "#fb8039")
    ) +
    tm_layout(legend.show = FALSE)
  tmap_save(district_only_map, paste0(district, " - 7. maps.png"))
}

adm1 <- distinct(nga_adm1, adm1)$ adm1
for (district in adm1) {
  wmr_one_page(nga_adm1, district)
}

wmr_one_page(nga_adm1, NA)

### WMR 2021 -----

wmr_2021_map <-
  tm_shape(wmr_2021_map_and_data) +
  tm_polygons("incidence_rate_all_age_rmean") +
  tm_facets(by = "Year")
print(wmr_2021_map)
tmap_save(wmr_2021_map, "Nigeria - country - Incidence_wmr - map.png")

## Rainfall -------
### Extract rainfall data -----
chirps_rainfall_rasters <- "/Users/sepmein/x/snt-data/Global/Data/CHIRPS_Global_raster_files/"
rainfall <- RasterResource$new(
  adm1_shapefile = nga_adm1_shapefile,
  local_destination = chirps_rainfall_rasters,
  output_destination = ".",
  index_name = "rainfall"
)

# View shapefile check adm1 name
View(st_read(nga_adm1_shapefile))
rainfall$load(
  target_adm_level = 1,
  adm1_name_in_shp = "State"
)

# clean rainfall data
rainfall$data |>
  mutate(
    year = str_extract(file, "\\d{4}"),
    month = str_extract(file, "\\d{2}(?=.tif)")
  ) |>
  select(adm1, year, month, value) |>
  rename(rainfall = value) |>
  write_csv("rainfall_nga_adm1.csv")

## add population graph
rainfall <- read_csv("rainfall_nga_adm1.csv")
rainfall <- rainfall |>
  mutate(
    date = lubridate::make_date(year = year, month = month)
  ) |>
  mutate(
    date = lubridate::floor_date(date, unit = "month")
  )

## country rainfall plot
rainfall |>
  group_by(date) |>
  summarise(rainfall = sum(rainfall)) |>
  ggplot(aes(x = date, y = rainfall)) +
  geom_line() +
  geom_point() +
  scale_x_date(NULL, date_labels = "%b %y", breaks = "month", guide = guide_axis(check.overlap = TRUE)) +
  theme(text = element_text(size = 5))

ggsave("nga_rainfall.png", height = 2.5, width = 4)

## fct rainfall plot
rainfall_line_plot <- function(data, district, country) {
  data <- data |>
    filter(adm1 == district)
  # group_by(date) |>
  # summarise(rainfall = sum(rainfall))

  ggplot(data, aes(x = date, y = rainfall)) +
    geom_line() +
    geom_point() +
    scale_x_date(NULL,
      date_labels = "%b %y",
      breaks = "month",
      guide = guide_axis(check.overlap = TRUE)
    ) +
    theme(text = element_text(size = 5))

  ggsave(
    paste0(
      district,
      " - 8 rainfall.png"
    ),
    height = 2.5,
    width = 4
  )
}
adm1 <- distinct(rainfall, adm1)$ adm1
for (district in adm1) {
  rainfall_line_plot(data = rainfall, district = district, country = "nga")
}

### Generate final indicators table
nga_adm1 |>
  group_by(year, adm1) |>
  filter(year == 2021) |>
  select(
    year, adm1, pop, conf
  ) |>
  summarise(pop = sum(pop), conf = sum(conf)) |>
  mutate(inci = conf / pop * 1000) |>
  write_csv("summary - 1 - malaria.csv")

nga_dhs_adm1 |>
  select(-Country, -Level) |>
  rename(year = Year, adm1 = State) |>
  write_csv("summary - 2 - survey.csv")

## SMC
smc_before_2020_data <- read_excel(smc_before_2020) |>
  select(adm1, year, smc1_num, smc2_num, smc3_num, smc4_num) |>
  group_by(adm1, year) |>
  summarise(
    smc1_num = sum(smc1_num, na.rm = TRUE),
    smc2_num = sum(smc2_num, na.rm = TRUE),
    smc3_num = sum(smc3_num, na.rm = TRUE),
    smc4_num = sum(smc4_num, na.rm = TRUE),
  )

smc_after_2020_data <- read_excel(
  smc_after_2020
) |>
  select(adm1, year, smc1_num, smc2_num, smc3_num, smc4_num)

nga_adm1_smc <- full_join(smc_before_2020_data, smc_after_2020_data) |>
  arrange(adm1, year) |>
  pivot_longer(cols = c(3:6)) |>
  mutate(year = as.factor(year))

adm1 <- distinct(nga_adm1_smc, adm1)$ adm1

## fct rainfall plot
smc_plot <- function(data, district, country) {
  data <- data |>
    filter(adm1 == district)
  # group_by(date) |>
  # summarise(rainfall = sum(rainfall))

  ggplot(data, aes(x = year)) +
    geom_bar(
      aes(fill = name, y = value),
      position = "dodge", group = "name",
      stat = "identity"
    ) +
    theme(
      legend.title = element_blank(),
      text = element_text(size = 5)
    ) +
    scale_fill_discrete(
      labels = c("SMC_1", "SMC_2", "SMC_3", "SMC_4")
    ) +
    labs(y = "Number of SMC")

  ggsave(
    paste0(
      district,
      " - 9 smc.png"
    ),
    height = 2.5,
    width = 4
  )
}

for (district in adm1) {
  smc_plot(
    data = nga_adm1_smc,
    district = district,
    country = "nga"
  )
}

# SMC country graph
nga_smc_plot <- nga_adm1_smc |>
  mutate(year = as.numeric(levels(year))[year]) |>
  group_by(year, name) |>
  summarise(
    value = sum(value)
  ) |>
  arrange(year) |>
  ggplot() +
  geom_bar(
    aes(x = year, fill = name, y = value),
    position = "dodge", group = "name",
    stat = "identity"
  ) +
  theme(
    legend.title = element_blank(),
    text = element_text(size = 5)
  ) +
  scale_fill_discrete(
    labels = c("SMC_1", "SMC_2", "SMC_3", "SMC_4")
  ) +
  labs(y = "Number of SMC")

ggsave(
  "nga - 9 smc.png",
  height = 2.5,
  width = 4
)

## LLINs
llins_before_2020_data <-
  read_xlsx(llins_before_2020) |>
  select(adm1, adm2, year, llins_num) |>
  group_by(adm1, year) |>
  summarise(llins_num = sum(llins_num, na.rm = TRUE))

llins_after_2020_data <-
  read_xlsx(llins_after_2020)

nga_adm1_llins <- full_join(llins_before_2020_data, llins_after_2020_data)

llins_plot <- function(data, district, country) {
  data <- data |>
    filter(adm1 == district)

  ggplot(data, aes(x = year)) +
    geom_bar(
      aes(y = llins_num),
      stat = "identity"
    ) +
    theme(text = element_text(size = 5))

  ggsave(
    paste0(
      district,
      " - 10 llins.png"
    ),
    height = 2.5,
    width = 4
  )
}

for (district in adm1) {
  llins_plot(
    data = nga_adm1_llins,
    district = district,
    country = "nga"
  )
}

# SMC country graph
nga_llins_plot <- nga_adm1_llins |>
  mutate(year = as.factor(year)) |>
  group_by(year) |>
  summarise(
    llins_num = sum(llins_num)
  ) |>
  ggplot() +
  geom_bar(
    aes(x = year, y = llins_num),
    stat = "identity"
  ) +
  theme(text = element_text(size = 5))

ggsave(
  "nga - 10 llins.png",
  height = 2.5,
  width = 4
)

## calculate nation indicator -------
nga_dhs_2021 <- nga_dhs |>
  filter(Year == 2021) |>
  mutate_at(c("State"), ~ str_replace(., "Akwa Ibom", "Akwa lbom")) |>
  left_join(pop, by = c("State" = "adm1"))

#  [6] "Malaria prevalence according to RDT"
nga_dhs_2021 |>
  mutate(prev_mic_pop = `Malaria prevalence according to RDT` * pop) |>
  summarise(prev_mic_pop = sum(prev_mic_pop), pop = sum(pop)) |>
  mutate(prev_rdt = prev_mic_pop / pop)

#  [7] "Malaria prevalence according to microscopy"
nga_dhs_2021 |>
  mutate(prev_mic_pop = `Malaria prevalence according to microscopy` * pop) |>
  summarise(prev_mic_pop = sum(prev_mic_pop), pop = sum(pop)) |>
  mutate(prev_rdt = prev_mic_pop / pop)

#  [8] "Persons with access to an insecticide-treated mosquito net (ITN)"
nga_dhs_2021 |>
  mutate(prev_mic_pop = `Persons with access to an insecticide-treated mosquito net (ITN)` * pop) |>
  summarise(
    prev_mic_pop = sum(prev_mic_pop, na.rm = TRUE),
    pop = sum(pop)
  ) |>
  mutate(prev_rdt = prev_mic_pop / pop)

#  [9] "Children who took any ACT"
nga_dhs_2021 |>
  mutate(prev_mic_pop = `Children who took any ACT` * pop_u5) |>
  summarise(prev_mic_pop = sum(prev_mic_pop), pop_u5 = sum(pop_u5)) |>
  mutate(prev_rdt = prev_mic_pop / pop_u5)

# [10] "Children tested for malaria with RDT"
nga_dhs_2021 |>
  mutate(prev_mic_pop = `Children tested for malaria with RDT` * pop_u5) |>
  summarise(prev_mic_pop = sum(prev_mic_pop), pop_u5 = sum(pop_u5)) |>
  mutate(prev_rdt = prev_mic_pop / pop_u5)
# [11] "Children with fever who had blood taken from a finger or heel for testing"
nga_dhs_2021 |>
  mutate(prev_mic_pop = `Children with fever who had blood taken from a finger or heel for testing` * pop_u5) |>
  summarise(prev_mic_pop = sum(prev_mic_pop), pop_u5 = sum(pop_u5)) |>
  mutate(prev_rdt = prev_mic_pop / pop_u5)
# [12] "Existing insecticide-treated mosquito nets (ITNs) used last night"
nga_dhs_2021 |>
  mutate(prev_mic_pop = `Existing insecticide-treated mosquito nets (ITNs) used last night` * pop) |>
  summarise(
    prev_mic_pop = sum(prev_mic_pop, na.rm = TRUE),
    pop = sum(pop)
  ) |>
  mutate(prev_rdt = prev_mic_pop / pop)

# [13] "Children under 5 who slept under any net"
nga_dhs_2021 |>
  mutate(prev_mic_pop = `Children under 5 who slept under any net` * pop_u5) |>
  filter(!is.na(`Children under 5 who slept under any net`)) |>
  summarise(prev_mic_pop = sum(prev_mic_pop, na.rm = TRUE), pop_u5 = sum(pop_u5)) |>
  mutate(prev_rdt = prev_mic_pop / pop_u5)
# [14] "Population who slept under an insecticide-treated mosquito net (ITN) last night"
nga_dhs_2021 |>
  mutate(prev_mic_pop = `Population who slept under an insecticide-treated mosquito net (ITN) last night` * pop) |>
  filter(!is.na(`Population who slept under an insecticide-treated mosquito net (ITN) last night`)) |>
  summarise(
    prev_mic_pop = sum(prev_mic_pop),
    pop = sum(pop)
  ) |>
  mutate(prev_rdt = prev_mic_pop / pop)

# [15] "Advice or treatment for fever sought from a health facility or provider"
nga_dhs_2021 |>
  mutate(prev_mic_pop = `Advice or treatment for fever sought from a health facility or provider` * pop) |>
  filter(!is.na(`Advice or treatment for fever sought from a health facility or provider`)) |>
  summarise(
    prev_mic_pop = sum(prev_mic_pop),
    pop = sum(pop)
  ) |>
  mutate(prev_rdt = prev_mic_pop / pop)
# [16] "Children under 5 who slept under an insecticide-treated net (ITN)"
nga_dhs_2021 |>
  mutate(prev_mic_pop = `Children under 5 who slept under an insecticide-treated net (ITN)` * pop_u5) |>
  filter(!is.na(`Children under 5 who slept under an insecticide-treated net (ITN)`)) |>
  summarise(prev_mic_pop = sum(prev_mic_pop), pop_u5 = sum(pop_u5)) |>
  mutate(prev_rdt = prev_mic_pop / pop_u5)

# malaria cases
nga_adm1 |> summarise(conf = sum(conf))

# malaria incidence
nga_adm1 |>
  mutate(adjinc3_pop = adjinc3 * pop) |>
  summarise(adjinc3_pop = sum(adjinc3_pop, na.rm = TRUE), pop = sum(pop)) |>
  mutate(adjinc3 = adjinc3_pop / pop)

# Indicators -------

wmr_2021 <- wmr_2021 |>
  mutate_at(c("Name"), ~ str_replace(., "Akwa lbom", "Akwa Ibom"))
## population ------
pop <- wmr_2021 |>
  filter(
    Year == 2021
  ) |>
  select(
    Name, Pop
  ) |>
  rename(
    State = Name
  )

## malaria rdt & microscopy density -------
dhs_2021 <- nga_dhs |>
  filter(Year == 2021, Level == "L2") |>
  select(-Country, -Survey, -Year)

indicator <- dhs_2021 |>
  left_join(pop) |>
  select(-Level)

## cases -------
wmr_cases <- wmr_2021 |>
  filter(Year == 2021) |>
  mutate(
    cases_mean = PAR * incidence_rate_all_age_rmean / 1000,
    cases_lci = PAR * incidence_rate_all_age_LCI / 1000,
    cases_uci = PAR * incidence_rate_all_age_LCI / 1000
  ) |>
  rename(State = Name) |>
  select(State, cases_mean, incidence_rate_all_age_rmean)

indicator <- indicator |>
  left_join(
    wmr_cases
  ) |>
  select(
    State,
    Pop,
    "Malaria prevalence according to RDT", "Malaria prevalence according to microscopy",
    cases_mean, incidence_rate_all_age_rmean,
    "Persons with access to an insecticide-treated mosquito net (ITN)",
    "Existing insecticide-treated mosquito nets (ITNs) used last night",
    "Population who slept under an insecticide-treated mosquito net (ITN) last night",
    "Children under 5 who slept under any net",
    "Children under 5 who slept under an insecticide-treated net (ITN)",
    "Advice or treatment for fever sought from a health facility or provider",
    "Children with fever who had blood taken from a finger or heel for testing"
  )

View(indicator)
indicator |>
mutate_if(is.numeric, ~round(.,digits=1)) |>
  write_csv(
    "nga_indicators.csv"
  )
