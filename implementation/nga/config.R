# Author: Chunzhe ZHANG zhangc@who.int, sepmein@gmail.com
# Added: 08/12/2022
# Updated: 08/12/2022

# Loading defaults from snt packages
library(snt)
# remote this line in production
devtools::load_all(path = "/Users/sepmein/x/snt")

snt::config()

# Change configuration for your own settings

# Rasters ---
## rainfall file path ---
path_rainfall_rasters <- "/Users/sepmein/x/snt-data/Global/Data/CHIRPS_Global_raster_files"
