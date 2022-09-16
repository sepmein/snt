# library("errorist")
# Never use library() or require() in a R package!

#' @export
set_country <- function(root_folder, country) {
  #  folder <- normalizePath(file.path(root_folder, country), mustWork = FALSE)
  setwd(root_folder)
  snt_country <<- country
}
#' @export
Resource <- R6::R6Class(
  # nolint
  classname = "resource",
  public = list(
    api_url = NULL,
    local_destination = NULL,
    output_destination = NULL,
    data = NULL,
    download_to = NULL,
    initialize = function(is_online,
                          is_batch,
                          api_url,
                          local_destination,
                          local_file_type,
                          output_destination,
                          download_to = NULL) {
      private$is_online <- is_online
      private$is_batch <- is_batch
      self$api_url <- api_url
      self$local_destination <- local_destination
      self$output_destination <- output_destination
      self$download_to <- download_to
      private$local_file_type <- local_file_type
      # check file type, type should be either csv/shapefile/raster
      if (is.null(private$check_local_file_type())) {
        stop(
          paste(
            "Creating Resource Object: ",
            "Type of local file should be ",
            "csv, shapefile or raster"
          )
        )
      }
    },
    download = function(select_files, destfile = NULL) {
      if (!(private$is_online)) {
        stop(paste(
          "Resource calling download. ",
          "But it is not an online resource"
        ))
      }
      if (private$is_batch) {
        print("called")
        private$download_batch(self$api_url, select_files, destfile)
      } else {
        private$download_single(self$api_url, select_files, destfile)
      }
      invisible(self)
    },
    load = function() {
      print(private$local_file_type)
      if (private$local_file_type == "raster") {
        private$load_raster()
      } else if (private$local_file_type == "shapefile") {
        private$load_shapefile()
      } else if (private$local_file_type == "csv") {
        private$load_csv()
      } else {
        stop(
          paste(
            "Resource loading raster files",
            ", but local_file_type is not raster/shapefile/csv."
          )
        )
      }
    },
    clean = function() {
    },
    export = function() {
    },
    plot_line = function() {
    },
    get_file_list = function() {
      # if (!(private$is_batch)) {
      #   stop(paste(
      #     "Resource get_file_list. ",
      #     "Should be batch"
      #   ))
      # }
      if (typeof(self$local_destination) == "list") {
        file_lists <- list()
        for (i in seq_along(self$local_destination)) {
          file_lists[[i]] <- list.files(path = self$local_destination[[i]])
        }
        return(file_lists)
      } else if (typeof(self$local_destination) == "character") {
        file_list <- list.files(path = self$local_destination)
        return(file_list)
      }
    },
    stack = function(country_resource, raster_to_dataframe_row_fn) {
      # load resource data
      # resource data type should be raster
      if (!(private$local_file_type == "raster")) {
        stop(paste(
          "CountryShapeFile stack with raster ",
          ", resource file type should be raster"
        ))
      }
      resource_file_list <- self$get_file_list()
      # for debug
      print(resource_file_list)
      # load shapefile into self$data object
      country_resource$load()
      country_shapefile <- country_resource$data
      if (typeof(self$local_destination) == "list") {
        # if local destination is a list
        # loop through all the list
        # save all files separately
        self$data <- list()
        for (i in seq_along(self$local_destination)) {
          resource_rasters <- NULL
          # browser()
          for (j in seq_along(resource_file_list[[i]])) {
            extracted <- raster::raster(
              file.path(
                self$local_destination[[i]],
                resource_file_list[[i]][[j]]
              )
            )
            if (is.null(resource_rasters)) {
              resource_rasters <- extracted
            } else {
              resource_rasters <- raster::stack(resource_rasters, extracted)
            }
          }
          stacked <- NULL
          # Extract raster values to list object
          for (k in seq_along(resource_file_list[[i]])) {
            extracted <- raster::raster(
              file.path(
                self$local_destination[[i]],
                resource_file_list[[i]][[k]]
              )
            )
            # browser()
            long <- raster_to_dataframe_row_fn(
              extracted,
              country_shapefile,
              resource_file_list[[i]][[k]],
              index = i
            )
            stacked <- rbind(stacked, long)
          }


          self$data[[i]] <- stacked
        }
      } else if ((typeof(self$local_destination == "character"))) {
        # if local destination is a string
        # load country resource(shapefile)
        # save data
        resource_rasters <- NULL
        for (i in seq_along(resource_file_list)) {
          extracted <- raster::raster(resource_file_list[i])
          resource_rasters <-
            raster::stack(resource_rasters, extracted)
        }

        stacked <- NULL
        # Extract raster values to list object
        for (j in seq_along(resource_file_list)) {
          extracted <- raster::raster(resource_file_list[j])
          r_vals <-
            raster::extract(extracted, country_shapefile)
          r_means <- lapply(r_vals, FUN = mean)
          long <- data.frame(unlist(r_means))
          long$name <- resource_file_list[j]
          long$district <- unique(self$data$NOM_DS)
          long$month <- as.numeric(substr(long$name, 18, 19))
          long$year <- as.numeric(substr(long$name, 13, 16))
          stacked <- rbind(stacked, long)
        }
        self$data <- stacked
      }
      invisible(self)
    }
  ),
  private = list(
    is_online = NULL,
    is_batch = NULL,
    local_file_type = NULL,
    check_local_file_type = function() {
      type <- switch(private$local_file_type,
        "csv" = "csv",
        "shapefile" = "shapefile",
        "raster" = "raster"
      )
      return(type)
    },
    load_raster = function(folder) {
      if (missing(folder)) {
        folder <- self$local_destination
      }
      if (private$is_batch) {
        # load multiple raster files
        if (typof(self$local_destination) == "list") {
          self$data <- list()
          for (i in seq_along(self$local_destination)) {
            # loop through different local destination
            raster_file_lists <-
              list.files(self$local_destination[i])
            raster_file_lists_stack <- NULL
            for (j in seq_along(raster_file_lists)) {
              target_raster <- raster::raster(raster_file_lists[j])
              raster_file_lists_stack <-
                raster::stack(
                  raster_file_lists_stack,
                  target_raster
                )
            }
            # store the stacked raster files into self$data as a list
            self$data[[i]] <- raster_file_lists_stack
          }
        } else if (typeof(self$local_destination == "character")) {
          raster_file_lists <- list.files(self$local_destination)
          raster_file_lists_stack <- NULL
          for (i in seq_along(raster_file_lists)) {
            target_raster <- raster::raster(raster_file_lists[i])
            raster_file_lists_stack <-
              raster::stack(
                raster_file_lists_stack,
                target_raster
              )
          }
          self$data <- raster_file_lists_stack
        }
      } else {
        # load single raster file
        # TODO to be implemented
      }
    },
    load_csv = function() {

    },
    load_shapefile = function() {
      # if (private$is_batch) {
      # load multiple shapefiles
      # TODO to be implemented
      # } else {
      self$data <- rgdal::readOGR(self$local_destination)
      # }
    },
    load_local = function() {

    },
    load_local_batch = function() {

    },
    download_single = function(api_url, select_files, destfile = NULL) {
      download_method <- private$choose_download_method(api_url)
      print(download_method)
      dest_file_path <- file.path(
        self$download_to,
        destfile
      )
      if (missing(api_url)) {
        api_url <- self$api_url
      }
      if (download_method == "ftp") {

      } else if (download_method == "http") {
        # TODO add download method for http
        stop(paste(
          "Resource download method ",
          "for http has not been implemented yet."
        ))
      } else if (download_method == "https") {
        download.file(api_url,
          dest_file_path,
          mode = "wb"
        )
        # get file ext
        if (file.exists(dest_file_path)) {
          file_ext <- dest_file_path
        } else {
          stop(
            paste(
              "File does not exists, ",
              "maybe download failed or user have removed file. ",
              "File path: ",
              dest_file_path
            )
          )
        }

        # if zip file
        if (file_ext == "zip") {
          private$unzip(
            dest_file_path,
            exdir = self$download_to
          )
        }
      }
    },
    download_batch = function(api_url, select_files, destfile = NULL) {
      download_method <- private$choose_download_method(api_url)
      print(download_method)
      if (missing(api_url)) {
        api_url <- self$api_url
      }
      if (download_method == "ftp") {
        # get files to be download
        filenames <- RCurl::getURL(api_url,
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
            try(RCurl::getBinaryURL(x, curl = con))
          }
        )
        names(contents) <- filenames[seq_along(contents)]

        for (j in seq_along(contents)) {
          writeBin(as.vector(contents[[j]]),
            con = basename(filenames[j])
          )
        }

        files <- str_sub(filenames, end = -1)
        for (i in seq_along(contents)) {
          R.utils::gunzip(basename(files[i]))
        }
      } else if (download_method == "http") {
        # TODO add download method for http
        stop(paste(
          "Resource download method ",
          "for http has not been implemented yet."
        ))
      } else if (download_method == "https") {
        download.file(api_url,
          file.path(self$download_to, destfile),
          mode = "wb"
        )
        # if zip file
        private$unzip(
          file.path(self$download_to, destfile),
          exdir = self$local_destination
        )
      }
    },
    choose_download_method = function(api_url) {
      # Download FTP
      if (startsWith(
        api_url,
        "ftp"
      )) {
        return("ftp")
      } else if (startsWith(
        api_url,
        "https"
      )) {
        return("https")
      } else if (startsWith(
        api_url,
        "http"
      )) {
        return("http")
      }
    },
    unzip = function(zip, to) {
      # unzip downloaded file
      utils::unzip(zip, exdir = to)
    }
  )
)

#' @export
Rainfall <- R6::R6Class(
  # nolint
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
        super$download_batch(
          api_url = self$africa_api,
          select_files = self$select_files(
            start_date,
            end_date
          )
        )
      } else if (target == "global") {
        super$download_batch(
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
    load = function(country_shapefile_resource,
                    country_adm2_code_resource) {
      super$stack(country_shapefile_resource)
      self$get_adm1_from_country(country_adm2_code_resource)
      invisible(self)
    },
    clean = function() {

    },
    export = function(type, filename) {
      setwd(self$output_destination)
      if (type == "stacked") {
        write.csv(self$data, file = filename)
      }
    },
    plot = function() {
      ggplot(self$data) +
        geom_line(aes(
          x = date,
          y = rain
        )) +
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
      rainfall_data_with_adm1 <- merge(self$data,
        country_adm2,
        by = "adm2"
      )
    }
  )
)

#' @export
CountryShapeFile <- R6::R6Class(
  # nolint
  "Country_Shapefile",
  inherit = Resource,
  public = list(
    is_online = NULL,
    is_batch = NULL,
    api_url = NULL,
    local_destination = NULL,
    local_file_type = NULL,
    output_destination = NULL,
    initialize = function(is_online = FALSE,
                          is_batch = FALSE,
                          api_url = NULL,
                          local_destination = paste(
                            "Countries",
                            snt_country,
                            "2020_SNT",
                            "Analysis",
                            "orig",
                            "data",
                            "shapefiles",
                            "District_sante_EPSG4326.shp",
                            sep = "/"
                          ),
                          local_file_type = "shapefile",
                          output_destination = ".") {
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

PlasmodiumIndex <- R6::R6Class(
  "PlasmodiumIndex",
  inherit = Resource,
  public = list(
    api_url = NULL,
    is_online = NULL,
    is_batch = NULL,
    local_file_type = NULL,
    with_95CI = NULL,
    download_to = NULL,
    plasmodium_index = NULL,
    initialize = function(is_online = TRUE,
                          is_batch = FALSE,
                          api_url = NULL,
                          local_file_type = "raster",
                          with_95CI = TRUE,
                          plasmodium = "pf",
                          index = "prevalence") {
      # TODO: change 2022 to the current year
      if (plasmodium == "pf") {
        plasmodium <- "Pf"
      } else if (plasmodium == "pv") {
        plasmodium <- "Pv"
      } else {
        stop("Plasmodium should be pf or pv")
      }
      if (index == "prevalence") {
        self$plasmodium_index <- paste(plasmodium, "PR")
      } else if (index == "incidence") {
        self$plasmodium_index <- paste(plasmodium, "IC")
      } else if (index == "mortality") {
        self$plasmodium_index <- paste(plasmodium, "MT")
      } else {
        stop("Index should be prevalence, incidence or mortality")
      }
      if (with_95CI) {
        local_destination <- list(
          paste(
            "Global/Data/MAP/2022_GBD_", plasmodium_index,
            "_estimates/Raster Data/", plasmodium_index, "_lci"
          ),
          paste(
            "Global/Data/MAP/2022_GBD_", plasmodium_index,
            "_estimates/Raster Data/", plasmodium_index, "_rmean"
          ),
          paste(
            "Global/Data/MAP/2022_GBD_", plasmodium_index,
            "_estimates/Raster Data/", plasmodium_index, "_uci"
          )
        )
      } else {
        local_destination <-
          paste(
            "Global/Data/MAP/2022_GBD_", plasmodium_index,
            "_estimates/Raster Data/", plasmodium_index, "_rmean"
          )
      }
      # TODO change the year 2020 to the current year
      output_destination <- file.path(
        "Countries", snt_country, "2020_SNT",
        "Analysis", "output", plasmodium_index
      )
      download_to <- file.path(
        "Global", "Data", "MAP", "2022_GBD_", plasmodium_index, "_estimates"
      )
      super$initialize(
        is_online,
        is_batch,
        api_url,
        local_destination,
        local_file_type,
        output_destination,
        download_to
      )
      self$api_url <-
        paste(
          "https://malariaatlas.org/wp-content/uploads/2022-gbd2020/",
          plasmodium_index, ".zip"
        )
      self$with_95CI <- with_95CI
      invisible(self)
    },
    load = function(country_shape_file) {
      # stack with country shape file
      super$stack(
        country_resource = country_shape_file,
        raster_to_dataframe_row_fn = self$raster_to_dataframe_row_fn
      )
      if (self$with_95CI) {
        # merge three dataframes into one
        self$data[[1]]$MEAN <- self$data[[2]]$MEAN
        self$data[[1]]$UCI <- self$data[[3]]$UCI
        self$data <- self$data[[1]]
      }
      invisible(self)
    },
    export = function() {
      if (typeof(self$data) == "list") {
        for (i in seq_along(self$local_destination)) {
          if (i == 1) {
            # LCI
            write.csv(
              self$data[[i]],
              file = file.path(
                self$output_destination,
                paste(self$plasmodium_index,
                  snt_country, "LCI.csv",
                  sep = "_"
                )
              )
            )
          } else if (i == 2) {
            # rMean
            write.csv(
              self$data[[i]],
              file = file.path(
                self$output_destination,
                paste(self$plasmodium_index,
                  snt_country, "MEAN.csv",
                  sep = "_"
                )
              )
            )
          } else if (i == 3) {
            # UCI
            write.csv(
              self$data[[i]],
              file = file.path(
                self$output_destination,
                paste(self$plasmodium_index,
                  snt_country, "UCI.csv",
                  sep = "_"
                )
              )
            )
          }
        }
      } else {
        write.csv(
          self$data,
          file = file.path(
            self$output_destination,
            paste(self$plasmodium_index,
              snt_country,
              ".csv",
              sep = "_"
            )
          )
        )
      }
      invisible(self)
    },
    load_csv = function() {
      self$data <- read.csv(
        file = file.path(
          self$output_destination,
          paste(self$plasmodium_index,
            snt_country,
            ".csv",
            sep = "_"
          )
        )
      )
    },
    plot_line = function() {
      # if with uncertainty
      # self$data will be a list
      if (self$with_95CI) {
        ggplot2::ggplot(self$data) +
          ggplot2::geom_line(
            ggplot2::aes(y = MEAN, x = year)
          ) +
          ggplot2::geom_ribbon(
            ggplot2::aes(
              ymin = LCI, ymax = UCI, y = MEAN, x = year
            ),
            alpha = 0.2
          ) +
          ggplot2::labs(x = self$plasmodium_index, title = snt_country) +
          ggplot2::facet_wrap(~district, ncol = 5)
      }
    }
  )
)

# TODO implement without 95 CI function
#' @export
PfPrevalence <- R6::R6Class(
  "PlasmodiunIndex",
  inherit = Resource,
  public = list(
    api_url = NULL,
    is_online = NULL,
    is_batch = NULL,
    local_file_type = NULL,
    with_95CI = NULL,
    download_to = NULL,
    initialize = function(is_online = TRUE,
                          is_batch = FALSE,
                          api_url = NULL,
                          local_file_type = "raster",
                          with_95CI = TRUE) {
      super$initialize(
                          is_online = TRUE,
                          is_batch = FALSE,
                          api_url = NULL,
                          local_file_type = "raster",
                          with_95CI = TRUE,
                          plasmodium = "pf",
                          index = "prevalence"
      )
      invisible(self)
    },
    raster_to_dataframe_row_fn = function(extracted,
                                          country_shapefile,
                                          resource_file_list,
                                          index) {
      r_vals <-
        raster::extract(extracted, country_shapefile)
      r_means <- lapply(r_vals, FUN = mean)
      long <- data.frame(unlist(r_means))
      long$name <- resource_file_list
      long$district <- unique(country_shapefile$NOM_DS)
      # The sequence of 95 CI with mean will be LCI, rMean, UCI
      if (index == 1 | index == 3) {
        # if LCI or UCI
        # example file naming will be
        # PfPR_UCI_Global_admin0_2000.tif
        # PfPR_LCI_Global_admin0_2000.tif
        colnames(long)[1] <- substr(long$name, 6, 8)
        long$year <- as.numeric(substr(long$name, 24, 27))
      } else if (index == 2) {
        colnames(long)[1] <- "MEAN"
        long$year <- as.numeric(substr(long$name, 26, 29))
      }
      return(long)
    }
  )
)

# TODO implement without 95 CI function
#' @export
PfIncidence <- R6::R6Class(
  "PfIncidence",
  inherit = PlasmodiumIndex,
  public = list(
    api_url = NULL,
    is_online = NULL,
    is_batch = NULL,
    local_file_type = NULL,
    with_95CI = NULL,
    download_to = NULL,
    initialize = function(is_online = TRUE,
                          is_batch = FALSE,
                          api_url = NULL,
                          local_file_type = "raster",
                          with_95CI = TRUE) {      super$initialize(
                          is_online = TRUE,
                          is_batch = FALSE,
                          api_url = NULL,
                          local_file_type = "raster",
                          with_95CI = TRUE,
                          plasmodium = "pf",
                          index = "incidence"
      )
      invisible(self)
    },
    raster_to_dataframe_row_fn = function(extracted,
                                          country_shapefile,
                                          resource_file_list,
                                          index) {
      r_vals <-
        raster::extract(extracted, country_shapefile)
      r_means <- lapply(r_vals, FUN = mean)
      long <- data.frame(unlist(r_means))
      long$name <- resource_file_list
      long$district <- unique(country_shapefile$NOM_DS)
      # The sequence of 95 CI with mean will be LCI, rMean, UCI
      if (index == 1 | index == 3) {
        # TODO change this after download file and check the folder structure
        # if LCI or UCI
        # example file naming will be
        # PfPR_UCI_Global_admin0_2000.tif
        # PfPR_LCI_Global_admin0_2000.tif
        colnames(long)[1] <- substr(long$name, 16, 18)
        long$year <- as.numeric(substr(long$name, 24, 27))
      } else if (index == 2) {
        colnames(long)[1] <- "MEAN"
        long$year <- as.numeric(substr(long$name, 26, 29))
      }
      return(long)
    }
  )
)

# TODO implement without 95 CI function
#' @export
PfMortality <- R6::R6Class(
  "PfMortality",
  inherit = PlasmodiumIndex,
  public = list(
    api_url = NULL,
    is_online = NULL,
    is_batch = NULL,
    local_file_type = NULL,
    with_95CI = NULL,
    download_to = NULL,
    initialize = function(is_online = TRUE,
                          is_batch = FALSE,
                          api_url = NULL,
                          local_file_type = "raster",
                          with_95CI = TRUE) {      super$initialize(
                          is_online = TRUE,
                          is_batch = FALSE,
                          api_url = NULL,
                          local_file_type = "raster",
                          with_95CI = TRUE,
                          plasmodium = "pf",
                          index = "mortality"
      )
      invisible(self)
    },
    raster_to_dataframe_row_fn = function(extracted,
                                          country_shapefile,
                                          resource_file_list,
                                          index) {
      r_vals <-
        raster::extract(extracted, country_shapefile)
      r_means <- lapply(r_vals, FUN = mean)
      long <- data.frame(unlist(r_means))
      long$name <- resource_file_list
      long$district <- unique(country_shapefile$NOM_DS)
      # The sequence of 95 CI with mean will be LCI, rMean, UCI
      if (index == 1 | index == 3) {
        # if LCI or UCI
        # example file naming will be
        # PfMT_UCI_Global_admin0_2000.tif
        # PfMT_LCI_Global_admin0_2000.tif
        colnames(long)[1] <- substr(long$name, 16, 18)
        long$year <- as.numeric(substr(long$name, 24, 27))
      } else if (index == 2) {
        colnames(long)[1] <- "MEAN"
        long$year <- as.numeric(substr(long$name, 26, 29))
      }
      return(long)
    }
  )
)

# TODO implement without 95 CI function
#' @export
PvPrevalence <- R6::R6Class(
  "PVPrevalence",
  inherit = PlasmodiumIndex,
  public = list(
    api_url = NULL,
    is_online = NULL,
    is_batch = NULL,
    local_file_type = NULL,
    with_95CI = NULL,
    download_to = NULL,
    initialize = function(is_online = TRUE,
                          is_batch = FALSE,
                          api_url = NULL,
                          local_file_type = "raster",
                          with_95CI = TRUE) {      super$initialize(
                          is_online = TRUE,
                          is_batch = FALSE,
                          api_url = NULL,
                          local_file_type = "raster",
                          with_95CI = TRUE,
                          plasmodium = "pv",
                          index = "prevalence"
      )
      invisible(self)
    },
    raster_to_dataframe_row_fn = function(extracted,
                                          country_shapefile,
                                          resource_file_list,
                                          index) {
      r_vals <-
        raster::extract(extracted, country_shapefile)
      r_means <- lapply(r_vals, FUN = mean)
      long <- data.frame(unlist(r_means))
      long$name <- resource_file_list
      long$district <- unique(country_shapefile$NOM_DS)
      # The sequence of 95 CI with mean will be LCI, rMean, UCI
      if (index == 1 | index == 3) {
        # if LCI or UCI
        # example file naming will be
        # PvPR_UCI_Global_admin0_2000.tif
        # PvPR_LCI_Global_admin0_2000.tif
        colnames(long)[1] <- substr(long$name, 6, 8)
        long$year <- as.numeric(substr(long$name, 24, 27))
      } else if (index == 2) {
        colnames(long)[1] <- "MEAN"
        long$year <- as.numeric(substr(long$name, 26, 29))
      }
      return(long)
    }
  )
)

# TODO implement without 95 CI function
#' @export
PvIncidence <- R6::R6Class(
  "PVIncidence",
  inherit = Resource,
  public = list(
    api_url = NULL,
    is_online = NULL,
    is_batch = NULL,
    local_file_type = NULL,
    with_95CI = NULL,
    download_to = NULL,
    initialize = function(is_online = TRUE,
                          is_batch = FALSE,
                          api_url = NULL,
                          local_file_type = "raster",
                          with_95CI = TRUE) {      super$initialize(
                          is_online = TRUE,
                          is_batch = FALSE,
                          api_url = NULL,
                          local_file_type = "raster",
                          with_95CI = TRUE,
                          plasmodium = "pf",
                          index = "prevalence"
      )
      invisible(self)
    },
    raster_to_dataframe_row_fn = function(extracted,
                                          country_shapefile,
                                          resource_file_list,
                                          index) {
      r_vals <-
        raster::extract(extracted, country_shapefile)
      r_means <- lapply(r_vals, FUN = mean)
      long <- data.frame(unlist(r_means))
      long$name <- resource_file_list
      long$district <- unique(country_shapefile$NOM_DS)
      # The sequence of 95 CI with mean will be LCI, rMean, UCI
      if (index == 1 | index == 3) {
        # TODO change this after download file and check the folder structure
        # if LCI or UCI
        # example file naming will be
        # PvPR_UCI_Global_admin0_2000.tif
        # PvPR_LCI_Global_admin0_2000.tif
        colnames(long)[1] <- substr(long$name, 16, 18)
        long$year <- as.numeric(substr(long$name, 24, 27))
      } else if (index == 2) {
        colnames(long)[1] <- "MEAN"
        long$year <- as.numeric(substr(long$name, 26, 29))
      }
      return(long)
    }
  )
)
