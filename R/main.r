library(R6)
library(RCurl)

Resource <- R6Class(
    classname = "resource",
    public = list(
        name = NULL,
        api = NULL,
        local_destination = NULL,
        data = NULL,
        initialize = function(name = NA,
                              api = NA,
                              local_destination = NA) {
            self$name <- name
            self$api <- api
            self$local_destination <- local_destination
        },
        download_batch = function() {
            filenames <- getURL(self$api,
                ftp.use.epsv = FALSE,
                dirlistonly = TRUE
            )
            filenames <- paste(self$api,
                strsplit(filenames, "\r*\n")[[1]],
                sep = ""
            )
            con <- getCurlHandle(
                ftp.use.epsv = FALSE,
                verbose = TRUE
            )
        }
    )
)
Rainfall <- R6Class(
    inherit = Resource,
    public = list(
        africa_api = "ftp://ftp.chg.ucsb.edu/pub/org/chg/products/CHIRPS-2.0/africa_monthly/tifs/",
        global_api = "ftp://ftp.chg.ucsb.edu/pub/org/chg/products/CHIRPS-2.0/global_monthly/tifs/",
        download_africa = function(start_date, end_date) {
            filenames <- getURL(self$africa_api,
                ftp.use.epsv = FALSE, dirlistonly = TRUE
            )
            filenames <- paste(self$africa_api,
                strsplit(filenames, "\r*\n")[[1]],
                sep = ""
            )
            con <- getCurlHandle(ftp.use.epsv = FALSE, verbose = TRUE)
            start_id <- grep(start_date, filenames)
            end_id <- grep(end_date, filenames)
            filenames <- filenames[start_id:end_id]
        },
        download_global = function(start_id, end_id) {
            filenames <- getURL(self$global_api,
                ftp.use.epsv = FALSE, dirlistonly = TRUE
            )
            filenames <- paste(self$global_api,
                strsplit(filenames, "\r*\n")[[1]],
                sep = ""
            )
            con <- getCurlHandle(ftp.use.epsv = FALSE, verbose = TRUE)
            start_id <- grep(start_date, filenames)
            end_id <- grep(end_date, filenames)
            filenames <- filenames[start_id:end_id]
        }
    )
)
rainfall <- Rainfall$new(
    name = "rainfall",
)

rainfall$download_africa()
