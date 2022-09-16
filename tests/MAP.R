library(snt)

# add year to this function
set_country(
    root_folder = "/Users/sepmein/dev/working/snt-data",
    country = "BDI"
)

bdi_shapefile <-
    CountryShapeFile$new()
# PfPR
pfpr <- PfPrevalence$new()$load_csv()
# Pf_prevalence$download(destfile = "pfPR.zip")
# added time when generated these data
# pfpr$load(country_shape_file = bdi_shapefile)$export()
# TODO
# for each districts
# shadows for ci
pfpr$plot_line()
# generate a map per year for mean, lci & uci
pfpr$plot_map(
  year = 2017,
  color = "Grean",
  categories = 6,
  labels = TRUE/FALSE
)

# PfIC
Pf_incidence <- PfIncidence$new()
Pf_incidence$download(destfile = "PfIC.zip")
Pf_incidence$load(country_shape_file = bdi_shapefile)$export()


# PfMT
Pf_mortality <- PfMortality$new()
Pf_mortality$download(destfile = "PfMT.zip")
Pf_mortality$load(country_shape_file = bdi_shapefile)$export()

# PvPR
Pv_prevalence <- PvPrevalence$new()
Pv_prevalence$download(destfile = "PvPR.zip")
Pv_prevalence$load(country_shape_file = bdi_shapefile)$export()

# PVIC
Pv_incidence <- PvIncidence$new()
Pv_incidence$download(destfile = "PvIC.zip")
Pv_incidence$load(country_shape_file = bdi_shapefile)$export()
