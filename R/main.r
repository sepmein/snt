library(R6)
library(RCurl)
library(lubridate) # for working with dates
library(ggplot2) # for creating graphs
library(scales) # to access breaks/formatting functions
library(gridExtra) # for arranging plots
library(grid) # for arranging plots
library(dplyr) # for subsetting by season
require(raster)
require(sp)
require(rgdal)
require(rts)
require(RCurl)
require(dplyr)
require(tidyr)
library(gdalUtils)

Resource <- R6Class(
    classname = "resource",
    public = list(
        api_url = NULL,
        local_destination = NULL,
        output_destination = NULL,
        data = NULL,
        local_file_type = NULL,
        initialize = function(is_online,
                              is_batch,
                              api_url,
                              local_destination,
                              local_file_type,
                              output_destination) {
            self$is_online <- is_online
            self$is_batch <- is_batch
            self$api_url <- api_url
            self$local_destination <- local_destination
            self$output_destination <- output_destination
            # check file type, type should be either csv/shapefile/raster
            if (
                is.null(
                    self$check_local_file_type(
                        local_file_type
                    )
                )
            ) {
                stop(
                    paste(
                        "Creating Resource Object: ",
                        "Type of local file should be ",
                        "csv, shapefile or raster"
                    )
                )
            }
            self$local_file_type <- local_file_type
        },
        download = function() {
            if (!self$is_online) {
                stop(paste(
                    "Resource calling download. ",
                    "But it is not an online resource"
                ))
            }
            if (self$is_batch) {
                self$download_batch()
            } else {
                self$download_single()
            }
        },
        load = function() {
            if (self$local_file_type == "raster") {
                self$load_raster()
            } else if (self$local_file_type == "shapefile") {
                self$load_shapefile()
            } else if (self$local_file_type == "csv") {
                self$load_csv()
            }
            stop(paste(
                "Resource loading raster files",
                ", but local_file_type is not raster/shapefile/csv."
            ))
        },
        clean = function() {

        },
        export = function() {

        },
        plot = function() {

        },
        get_file_list = function() {
            if (!(self$is_batch)) {
                stop(paste(
                    "Resource get_file_list. ",
                    "Should be batch"
                ))
            }
            setwd(self$local_destination)
            return(list.files())
        }
    ),
    private = list(
        is_online = NULL,
        is_batch = NULL,
        check_local_file_type = function() {
            type <- switch(self$local_file_type,
                "csv" = "csv",
                "shapefile" = "shapefile",
                "raster" = "raster"
            )
            return(type)
        },
        load_raster = function() {
            if (self$is_batch) {
                # load multiple raster files
                setwd(self$local_destination)
                raster_file_lists <- list.files()
                raster_file_lists_stack <- NULL
                for (i in seq_along(raster_file_lists)) {
                    target_raster <- raster(raster_file_lists[i])
                    raster_file_lists_stack <- stack(
                        raster_file_lists_stack,
                        target_raster
                    )
                }
                self$data <- raster_file_lists_stack
            } else {
                # load single raster file
                # TODO to be implemented
            }
        },
        load_csv = function() {

        },
        load_shapefile = function() {
            if (self$is_batch) {
                # load multiple shapefiles
                # TODO to be implemented
            } else {
                self$data <- readOGR(self$local_destination)
            }
        },
        load_local = function() {},
        load_local_batch = function() {},
        download_single = function() {
        },
        download_batch = function(api_url, select_files) {
            download_method <- self$choose_download_method()
            if (missing(api_url)) {
                api_url <- self$api_url
            }
            if (download_method == "ftp") {
                # get files to be download
                filenames <- getURL(api_url,
                    ftp.use.epsv = FALSE,
                    dirlistonly = TRUE
                )
                # Deal with newlines as \n or \r\n. (BDR)
                filenames <- paste(api_url,
                    strsplit(filenames, "\r*\n")[[1]],
                    sep = ""
                )

                if (!is.null(select_files)) {
                    filenames <- select_files(filenames)
                }

                # there is a slight possibility that some of the files that are
                # returned in the directory listing and in filenames will disappear
                # when we go back to get them.
                # So we use a try() in the call getURL.
                contents <- sapply(
                    filenames,
                    function(x) {
                        try(
                            getBinaryURL(x, curl = con)
                        )
                    }
                )
                names(contents) <- filenames[seq_along(contents)]

                for (j in seq_along(contents)) {
                    writeBin(
                        as.vector(contents[[j]]),
                        con = basename(filenames[j])
                    )
                }

                files <- str_sub(filenames, end = -1)
                for (i in seq_along(contents)) {
                    gunzip(basename(files[i]))
                }
            } else if (download_method == "http") {
                # TODO add download method for http
                stop(paste(
                    "Resource download method ",
                    "for http has not been implemented yet."
                ))
            }
        },
        choose_download_method = function() {
            # Download FTP
            if (startsWith(
                self$api_url,
                "ftp",
                ignore.case = TRUE
            )) {
                return("ftp")
            }
        }
    )
)

Rainfall <- R6Class(
    classname = "Rainfall",
    inherit = Resource,
    public = list(
        africa_api = "ftp://ftp.chg.ucsb.edu/pub/org/chg/products/CHIRPS-2.0/africa_monthly/tifs/",
        global_api = "ftp://ftp.chg.ucsb.edu/pub/org/chg/products/CHIRPS-2.0/global_monthly/tifs/",
        initialize = function(is_online = TRUE,
                              is_batch = TRUE,
                              api_url = NULL,
                              local_destination,
                              local_file_type = "raster",
                              output_destination) {
            super$initialize(
                is_online,
                is_batch,
                api_url,
                local_destination,
                local_file_type,
                output_destination
            )
            invisible(self)
        },
        download = function(target,
                            path_to_save,
                            start_date,
                            end_date) {
            setwd(path_to_save)
            if (target == "africa") {
                super$download(
                    api_url = self$africa_api,
                    select_files = self.select_files(
                        start_date,
                        end_date
                    )
                )
            } else if (target == "global") {
                super$download(
                    api_url = self$africa_api,
                    select_files = self.select_files(
                        start_date,
                        end_date
                    )
                )
            } else {
                stop(paste(
                    "Rainfall Resource Download, ",
                    "target should be africa or global"
                ))
            }
            invisible(self)
        },
        load <- function(country_shapefile_resource,
                         country_adm2_code_resource) {
            self$stack(country_shapefile_resource)
            self$get_adm1_from_country(country_adm2_code_resource)
            invisible(self)
        },
        clean <- function() {
        },
        export = function(type, filename) {
            setwd(self$output_destination)
            if (type == "stacked") {
                write.csv(self$data, file = filename)
            }
        },
        plot = function() {
            ggplot(self$data) +
                geom_line(
                    aes(
                        x = date,
                        y = rain
                    )
                ) +
                facet_wrap(~adm1)
            invisible(self)
        }
    ),
    private = list(
        is_online = TRUE,
        is_batch = TRUE,
        local_file_type = "raster",
        stacked_data = NULL,
        select_files = function(start_date, end_date) {
            return(function(filenames) {
                start_id <- grep(start_date, filenames)
                end_id <- grep(end_date, filenames)
                filenames <- filenames[start_id:end_id]
            })
        },
        get_adm1_from_country = function(path_to_country_adm2) {
            self$data <- rename(self$data, amd2 = district)
            self$data <- rename(self$data, rain = unlistrmeans)
            self$data$date <- str_c(self$data$year,
                str_pad(self$data$month, 2, pad = 0),
                sep = "-"
            )
            self$data$date <- as.Date(sef.data$date)
            country_adm2 <- read_dta(path_to_country_adm2)
            rainfall_data_with_adm1 <- merge(
                self$data,
                country_adm2,
                by = "adm2"
            )
        },
        stack = function(country_resource) {
            # load resource data
            # resource data type should be raster
            if (!(self$local_file_type == "raster")) {
                stop(paste(
                    "CountryShapeFile stack with raster ",
                    ", resource file type should be raster"
                ))
            }
            resource_file_list <- self$get_file_list()

            resource_rasters <- NULL

            for (i in seq_along(resource_file_list)) {
                extracted <- raster(resource_file_list[i])
                resource_rasters <- stack(resource_rasters, extracted)
            }

            # load shapefile into self$data object
            country_resource$load()
            country_shapefile <- country_resource$data

            stacked <- NULL
            # Extract raster values to list object
            for (j in seq_along(resource_file_list)) {
                extracted <- raster(resource_file_list[j])
                r_vals <- raster::extract(extracted, country_shapefile)
                r_means <- lapply(r_vals, FUN = mean)
                long <- data.frame(unlist(r_means))
                long$name <- resource_file_list[j]
                long$district <- unique(self$data$NOM_DS)
                long$month <- as.numeric(substr(long$name, 18, 19))
                long$year <- as.numeric(substr(long$name, 13, 16))
                stacked <- rbind(stacked, long)
            }
            self$data <- stacked
            invisible(self)
        }
    )
)

CountryShapeFile <- R6Class(
    "Country_Shapefile",
    inherit = Resource,
    public = list(
        stacked_data = NULL,
        initialize = function(is_online = FALSE,
                              is_batch = FALSE,
                              api_url = NULL,
                              local_destination,
                              local_file_type = "shapefile",
                              output_destination) {
            super$initialize(
                is_online,
                is_batch,
                api_url,
                local_destination,
                local_file_type,
                output_destination
            )
        }
    )
)

# implementation
# Download rainfall data
gha_shapefile <- CountryShapeFile$new(
    local_destination = "/Users/sepmein/Library/CloudStorage/OneDrive-共享的库-WorldHealthOrganization/GMP-SIR\ -\ Country_Analytical_Support/Countries/BDI/2020_SNT/Analysis/orig/data/shapefiles/Province_EPSG4326.shp",
)

rainfall <- Rainfall$new(
    local_destination = "",
    output_destination = "",
    downloaded=TRUE
)$
    download(
    target = "africa",
    path_to_save = "./africa",
    start_date = 2021.05,
    end_date = 2022.06
)$
    download(
    target = "global",
    path_to_save = "./global",
    start_date = 2021.05,
    end_date = 2022.06
)
##rainfall$download()
rainfall$
    load(
    country_shapefile_resource = gha_shapefile,
    country_adm2_code_resource = "/Users/sepmein/Library/CloudStorage/OneDrive- 共享的库-WorldHealthOrganization/GMP-SIR\ -\ Country_Analytical_Support/Countries/BDI/2020_SNT/Analysis/orig/data/country_adm2_codes.dta"
)
rainfall$plot()
# extract csv
# summary