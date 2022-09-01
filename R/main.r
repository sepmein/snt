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
    private = list(
        download_filenames = NULL,
    ),
    public = list(
        africa_api = "ftp://ftp.chg.ucsb.edu/pub/org/chg/products/CHIRPS-2.0/africa_monthly/tifs/",
        global_api = "ftp://ftp.chg.ucsb.edu/pub/org/chg/products/CHIRPS-2.0/global_monthly/tifs/",
        download_batch = function(api, start_date, end_date) {
            filenames <- getURL(api,
                ftp.use.epsv = FALSE, dirlistonly = TRUE
            )
            filenames <- paste(api,
                strsplit(filenames, "\r*\n")[[1]],
                sep = ""
            )
            con <- getCurlHandle(ftp.use.epsv = FALSE, verbose = TRUE)
            start_id <- grep(start_date, filenames)
            end_id <- grep(end_date, filenames)
            filenames <- filenames[start_id:end_id]
            # there is a slight possibility that some of the files that are
            # returned in the directory listing and in filenames will disappear
            # when we go back to get them. So we use a try() in the call getURL.
            contents <- sapply(
                filenames,
                function(x) try(getBinaryURL(x, curl = con))
            )
            names(contents) <- filenames[seq_along(contents)]

            for (j in seq_along(contents)) {
                writeBin(as.vector(contents[[j]]), con = basename(filenames[j]))
            }

            files <- str_sub(filenames, end = -1)

            for (i in seq_along(contents)) {
                gunzip(basename(files[i]))
            }
        },
        download_africa = function(start_id, end_id) {
            self$download_batch(api = self$africa_api, start_id, end_id)
        },
        download_global = function(start_id, end_id) {
            self$download_batch(api = self$global_api, start_id, end_id)
        }
    )
)

rainfall <- Rainfall$new(
    name = "rainfall",
)

rainfall$download_africa()
