library(snt)

# add year to this function
set_country(
  root_folder = "/Users/sepmein/dev/working/snt-data",
  country = "BDI"
)
bdi_shapefile <-
  paste(
    "Countries", "BDI", "2020_SNT", "Analysis", "orig", "data",
    "shapefiles",
    "District_sante_EPSG4326.shp",
    sep = "/"
  )
# PfPR
pfpr <- PfPrevalence$new(
  api_url = "https://malariaatlas.org/wp-content/uploads/2022-gbd2020/pfpr.zip",
  with_95CI = TRUE
)
# pfpr$download(destfile = "pfPR.zip")
# added time when generated these data
pfpr$load(bdi_shapefile)$export()
pfpr$load_csv()
# per plot for province province_district in title
pfpr$plot_line()
# generate a map per year for mean, lci & uci
# add manually breaks
pfpr$plot_map(year = 2013, categories = 6)
# plot all years
pfpr$plot_map(categories = 8, palette = "viridis")

# PfIC
Pf_incidence <- PfIncidence$new()
# Pf_incidence$download(destfile = "PfIC.zip")
Pf_incidence$load(country_shape_file = bdi_shapefile)
Pf_incidence$export()
Pf_incidence$plot_line()

# PfMT
Pf_mortality <- PfMortality$new()
# Pf_mortality$download(destfile = "PfMT.zip")
Pf_mortality$load(country_shape_file = bdi_shapefile)$export()

# PvPR
Pv_prevalence <- PvPrevalence$new()
# Pv_prevalence$download(destfile = "PvPR.zip")
Pv_prevalence$load(country_shape_file = bdi_shapefile)$export()

# PVIC
Pv_incidence <- PvIncidence$new()
# Pv_incidence$download(destfile = "PvIC.zip")
Pv_incidence$load(country_shape_file = bdi_shapefile)$export()
