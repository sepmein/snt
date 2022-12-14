# Author: Chunzhe ZHANG zhangc@who.int, sepmein@gmail.com
# Added: 08/12/2022
# Updated: 08/12/2022

# Loading defaults from snt packages
library(snt)
# remote this line in production
devtools::load_all(path = "/Users/sepmein/x/snt")

config <- snt::config(
    country = "NGA",
    raster_root = "/Users/sepmein/x/snt-data/Global/Data",
    rainfall_folder = "CHIRPS_Global_raster_files",
    shapefile = "/Users/sepmein/Library/CloudStorage/OneDrive-SharedLibraries-WorldHealthOrganization/GMP-SIR\ -\ Country_Analytical_Support/Countries/NGA/WHO_NGA/NGA_2022_SNT/_Submitted_data/Shapefiles/NGA_LGAs3.shp"
)

# Change configuration for your own settings

# Rasters ---
## rainfall file path ---
rainfall_folder <- config$raster$rainfall$folder
