library(snt)

set_country(
    root_folder = "/Users/sepmein/dev/working/snt-data",
    country = "BDI"
)

bdi_shapefile <-
    CountryShapeFile$new()

# PfPR
Pf_prevalence <- PfPrevalence$new()
Pf_prevalence$download(destfile = "pfPR.zip")
Pf_prevalence$load(country_shape_file = bdi_shapefile)$export()

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