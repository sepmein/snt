# git clone https://github.com/WorldHealthOrganization/sng.git
# devtools::install()
# devtools::install_github("WorldHealthOrganization/snt.git")

library(snt)
library(ggplot2)
library(tmap)

set_country(
    root_folder = "/Users/sepmein/dev/working/snt-data",
    country = "BDI"
)

bdi_health_facilities <- sf::st_read(
    "Countries//BDI//2020_SNT//Analysis//orig//data//shapefiles//BDI_HFs_2013.shp"
)
bdi_districts <- sf::st_read(
  "Countries//BDI//2020_SNT//Analysis//orig//data//shapefiles//District_sante_EPSG4326.shp"
)
my_data <- read.csv("Countries//BDI//2020_SNT//Analysis//output//Pf_prevalence//prevalence_BDI_MEAN.csv")
my_data <- dplyr::filter(my_data, year == 2020)
my_data <- dplyr::rename(my_data, Pf_prevalence = unlist.r_means.)
bdi_districts <- dplyr::rename(bdi_districts, district = NOM_DS)
map_and_data <- dplyr::inner_join(bdi_districts, my_data
                  )
tm_shape(map_and_data) +
  tm_borders() +
  tm_fill(col="Pf_prevalence"
           ) + tmap_options(max.categories = 2)
# ggplot(my_map) +
# geom_sf(aes())

tm_shape(my_map) +
    tm_dots()

tmap_mode("view")
tmap_last()
