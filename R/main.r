# Resource ----------------------------------------------------------------
#' @export
Resource <- R6::R6Class(
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
          paste0(
            "Creating Resource Object: ",
            "Type of local file should be ",
            "csv, shapefile or raster"
          )
        )
      }
    },
    download = function(select_files, destfile = NULL) {
      if (!(private$is_online)) {
        stop(paste0(
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
          paste0(
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
      if (typeof(self$local_destination) == "list") {
        # 如果local_destination是一个list，
        # 那么默认用户设置的是数个文件夹path
        # 返回二级列表
        file_lists <- list()
        for (i in seq_along(self$local_destination)) {
          file_lists[[i]] <- list.files(path = self$local_destination[[i]])
        }
        return(file_lists)
      } else if (typeof(self$local_destination) == "character") {
        # 如果local_destination是一个字符串
        # 则用户设置的是一个文件夹path
        # 返回该文件夹下的所有文件列表
        file_list <- list.files(path = self$local_destination)
        return(file_list)
      }
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
        stop(paste0(
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
            paste0(
              "File does not exists, ",
              "maybe download failed or user have removed file. ",
              "File path: ",
              dest_file_path
            )
          )
        }

        # if zip file
        if (file_ext == "zip") {
          private$unzip(dest_file_path,
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
        stop(paste0(
          "Resource download method ",
          "for http has not been implemented yet."
        ))
      } else if (download_method == "https") {
        download.file(api_url,
          file.path(self$download_to, destfile),
          mode = "wb"
        )
        # if zip file
        private$unzip(file.path(self$download_to, destfile),
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

# Shapefiles --------------------------------------------------------------

# Raster Resource----------------------------------------------------------
#' @export
RasterResource <- R6::R6Class(
  "RasterResource",
  inherit = Resource,
  public = list(
    api_url = NULL,
    is_online = NULL,
    is_batch = NULL,
    local_file_type = NULL,
    with_95CI = NULL,
    download_to = NULL,
    adm0_shapefile = NULL,
    adm1_shapefile = NULL,
    adm2_shapefile = NULL,
    adm3_shapefile = NULL,
    target_adm_level = NULL,
    index_name = NULL,
    cores = NULL,
    initialize = function(adm0_shapefile = NULL,
                          adm1_shapefile = NULL,
                          adm2_shapefile = NULL,
                          adm3_shapefile = NULL,
                          with_95CI = FALSE,
                          is_online = FALSE,
                          is_batch = FALSE,
                          api_url = NULL,
                          local_destination = NULL,
                          output_destination = NULL,
                          local_file_type = "raster",
                          download_to = NULL,
                          index_name = NULL,
                          ...) {
      super$initialize(
        is_online = is_online,
        is_batch = is_batch,
        api_url = api_url,
        local_file_type = local_file_type,
        download_to = download_to,
        local_destination = local_destination,
        output_destination = output_destination,
        ...
      )
      self$with_95CI <- with_95CI
      self$index_name <- index_name
      self$adm0_shapefile <- adm0_shapefile
      self$adm1_shapefile <- adm1_shapefile
      self$adm2_shapefile <- adm2_shapefile
      self$adm3_shapefile <- adm3_shapefile
      self$cores <- parallel::detectCores()
    },
    load_shp = function(shp) {
      shp <- sf::st_read(shp)
      # test if GUID column exist
      # if not generate GUID column
      if (!("GUID" %in% names(shp))) {
        shp <- shp |> dplyr::mutate(GUID = uuid::UUIDgenerate())
      }
      return(shp)
    },
    load = function(target_adm_level = 2,
                    adm0_name_in_shp = NULL,
                    adm1_name_in_shp = NULL,
                    adm2_name_in_shp = NULL,
                    adm3_name_in_shp = NULL,
                    method = "mean") {
      self$target_adm_level <- target_adm_level
      # set target shapefile based on target level
      if (target_adm_level == 0) {
        target_shapefile <- self$adm0_shapefile
      } else if (target_adm_level == 1) {
        target_shapefile <- self$adm1_shapefile
      } else if (target_adm_level == 2) {
        target_shapefile <- self$adm2_shapefile
      } else {
        target_shapefile <- self$adm3_shapefile
      }

      # load resource data
      # resource data type should be raster
      if (!(private$local_file_type == "raster")) {
        stop(paste(
          "CountryShapeFile stack with raster ",
          ", resource file type should be raster"
        ))
      }

      # read shapefile
      loaded_shp <- self$load_shp(target_shapefile)


      if (typeof(self$local_destination) == "list") {
        # if local_destination is a list of folder
        self$data <- self$load_multiple_folder(
          target_adm_level,
          adm0_name_in_shp,
          adm1_name_in_shp,
          adm2_name_in_shp,
          adm3_name_in_shp,
          method,
          loaded_shp
        )
      } else if (dir.exists(self$local_destination)) {
        # if is a single folder
        self$data <- self$load_single_folder(
          target_adm_level,
          adm0_name_in_shp,
          adm1_name_in_shp,
          adm2_name_in_shp,
          adm3_name_in_shp,
          method,
          loaded_shp
        )
      } else if (file_test("-f", self$local_destination)) {
        # if is a single file
        self$data <- self$load_single_file(
          target_adm_level,
          adm0_name_in_shp,
          adm1_name_in_shp,
          adm2_name_in_shp,
          adm3_name_in_shp,
          method,
          loaded_shp
        )
      } else {
        stop("Local destination should either be a list of folder, a folder or a file name") # nolint
      }
      self$clean()
      # if (self$with_95CI) {
      # merge three dataframes into one
      # self$data[[1]]$MEAN <- self$data[[2]]$MEAN
      # self$data[[1]]$UCI <- self$data[[3]]$UCI
      # self$data <- self$data[[1]]
      # }
      invisible(self)
    },
    clean = function() {

    },
    get_district_raster_data = function(method,
                                        extracted_raster_data) {
      # mean method in R is slow
      # changed to this method based on
      # https://stackoverflow.com/a/18604487/886198
      if (method == "mean") {
        result <- parallel::mclapply(extracted_raster_data,
          FUN = snt::faster_mean,
          mc.cores = self$cores
        )
      }
      return(result)
    },
    load_single_file = function(target_adm_level,
                                adm0_name_in_shp,
                                adm1_name_in_shp,
                                adm2_name_in_shp,
                                adm3_name_in_shp,
                                method,
                                loaded_shp,
                                the_file = NULL) {
      # single raster file
      # set current file, default is self$local_destination
      if (is.null(the_file)) {
        current_file <- self$local_destination
      } else {
        current_file <- the_file
      }

      # extract raster data
      raster_data <-
        raster::raster(file.path(current_file))
      # into shapefile
      extracted_raster_data <-
        raster::extract(raster_data, loaded_shp)
      # aggregate
      by_district <- self$get_district_raster_data(
        method,
        extracted_raster_data
      )
      # tibble
      result <- tibble::as_tibble(unlist(by_district))
      # set file for tibble
      result$file <- current_file
      result$GUID <- loaded_shp$GUID

      result <- dplyr::left_join(loaded_shp, result, by = c("GUID"))

      # set adm1, adm2, adm3 from shp
      # if (target_adm_level == 0) {
      #   result$adm0 <- loaded_shp[[adm0_name_in_shp]]
      # } else if (target_adm_level == 1) {
      #   if (!is.null(adm0_name_in_shp)) {
      #     result$adm0 <-
      #       loaded_shp[[adm0_name_in_shp]]
      #   }
      #   result$adm1 <- loaded_shp[[adm1_name_in_shp]]
      # } else if (target_adm_level == 2) {
      #   if (!is.null(adm0_name_in_shp)) {
      #     result$adm0 <-
      #       loaded_shp[[adm0_name_in_shp]]
      #   }
      #   if (!is.null(adm1_name_in_shp)) {
      #     result$adm1 <-
      #       loaded_shp[[adm1_name_in_shp]]
      #   }
      #   result$adm2 <- loaded_shp[[adm2_name_in_shp]]
      # } else if (target_adm_level == 3) {
      #   if (!is.null(adm0_name_in_shp)) {
      #     result$adm0 <-
      #       loaded_shp[[adm0_name_in_shp]]
      #   }
      #   if (!is.null(adm1_name_in_shp)) {
      #     result$adm1 <-
      #       loaded_shp[[adm1_name_in_shp]]
      #   }
      #   if (!is.null(adm2_name_in_shp)) {
      #     result$adm2 <-
      #       loaded_shp[[adm2_name_in_shp]]
      #   }
      #   result$adm3 <- loaded_shp[[adm3_name_in_shp]]
      # }

      # return result
      return(result)
    },
    load_single_folder = function(target_adm_level,
                                  adm0_name_in_shp,
                                  adm1_name_in_shp,
                                  adm2_name_in_shp,
                                  adm3_name_in_shp,
                                  method,
                                  loaded_shp,
                                  folder = NULL) {
      # multiple raster files
      if (is.null(folder)) {
        raster_files <- self$get_file_list()
        folder <- self$local_destination
      } else {
        raster_files <- list.files(path = folder)
        folder <- folder
      }
      # if local destination is a string
      # load country resource(shapefile)
      browser()
      result <- NULL
      # Extract raster values to list object
      for (j in seq_along(raster_files)) {
        the_file <- file.path(
          folder,
          raster_files[j]
        )
        loaded <- self$load_single_file(
          target_adm_level,
          adm0_name_in_shp,
          adm1_name_in_shp,
          adm2_name_in_shp,
          adm3_name_in_shp,
          method,
          loaded_shp,
          the_file
        )
        result <- dplyr::bind_rows(result, loaded)
      }
      return(result)
    },
    load_multiple_folder = function(target_adm_level,
                                    adm0_name_in_shp,
                                    adm1_name_in_shp,
                                    adm2_name_in_shp,
                                    adm3_name_in_shp,
                                    method,
                                    loaded_shp) {
      result <- list()
      for (i in seq_along(self$local_destination)) {
        loaded <- self$load_single_folder(
          target_adm_level,
          adm0_name_in_shp,
          adm1_name_in_shp,
          adm2_name_in_shp,
          adm3_name_in_shp,
          method,
          loaded_shp,
          folder =
            self$local_destination[[i]]
        )
        result[[i]] <- loaded
      }
      return(result)
    },
    export = function(to = NULL, filename) {
      if (is.null(to)) {
        to <- self$output_destination
      }
      if (typeof(self$local_destination) == "list") {
        for (i in seq_along(self$local_destination)) {
          filename <- paste0(filename, "_", i)
          write.csv(self$data, file = file.path(to, filename))
        }
      } else if (dir.exists(self$local_destination)) {
        write.csv(self$data, file = file.path(to, filename))
      } else if (file_test("-f", self$local_destination)) {
        write.csv(self$data, file = file.path(to, filename))
      } else {
        stop("Local destination should either be a list of folder, a folder or a file name") # nolint
      }
      invisible(self)
    },
    read_csv = function(from = NULL, filename) {
      if (is.null(from)) {
        from <- self$output_destination
      }
      self$data <- read.csv(file = file.path(from, filename))
    },
    plot_map = function(palette = "YlGn",
                        reverse_color_order = FALSE,
                        categories = 5,
                        breaks = NULL,
                        label = TRUE,
                        adm1_border_thickness = 5,
                        adm2_border_thickness = 3,
                        adm3_border_thickness = 1,
                        adm1_border_color = "black",
                        adm2_border_color = "black",
                        adm3_border_color = "grey",
                        adm1_name_in_shp = NULL,
                        adm2_name_in_shp = NULL,
                        adm3_name_in_shp = NULL,
                        title = FALSE) {
      # merge shape file with data
      index <- self$index_name
      my_data <- self$data
      tmap::tmap_options(check.and.fix = TRUE)

      if (!is.null(self$adm1_shapefile)) {
        adm1_shapefile <- sf::st_read(self$adm1_shapefile)
        dplyr::rename(adm1_shapefile, adm1 = !!adm1_name_in_shp)
      }
      if (!is.null(self$adm2_shapefile)) {
        adm2_shapefile <- sf::st_read(self$adm2_shapefile)
        adm2_shapefile <- adm2_shapefile |>
          dplyr::rename(adm2 = !!adm2_name_in_shp)
      }
      if (!is.null(self$adm3_shapefile)) {
        adm3_shapefile <- sf::st_read(self$adm3_shapefile)
        adm3_shapefile <- adm3_shapefile |>
          dplyr::rename(adm3 = !!adm3_name_in_shp)
      }

      if (file_test("-f", self$local_destination)) {
        map_and_data <- switch(self$target_adm_level,
          "1" = dplyr::inner_join(adm1_shapefile, my_data),
          "2" = dplyr::inner_join(adm2_shapefile, my_data),
          "3" = dplyr::inner_join(adm3_shapefile, my_data)
        )
        main_color <- switch(self$target_adm_level,
          "1" = adm1_border_color,
          "2" = adm2_border_color,
          "3" = adm3_border_color,
        )

        if (length(palette) == 1) {
          # if palette is "foo"
          palette <- rev(grDevices::hcl.colors(
            categories,
            palette
          ))
        } else if (length(palette) > 1) {
          # if palette is c('foo','bar')
          palette <- palette
        }

        # plot specified year
        map <- tmap::tm_shape(map_and_data) +
          tmap::tm_borders(col = main_color) +
          tmap::tm_fill(index,
            n = categories,
            palette = palette,
            breaks = breaks
          )

        if (is.logical(title)) {
          if (title == TRUE) {
            map <- map + tmap::tm_layout(title = paste(snt_country, index,
              sep = "-"
            ))
          }
        } else if (is.character(title)) {
          map <- map + tmap::tm_layout(title = title)
        }

        if (self$target_adm_level == 2) {
          if (!is.null(self$adm1_shapefile)) {
            map <- map +
              tmap::tm_shape(adm1_shapefile) +
              tmap::tm_borders(lwd = adm1_border_thickness, col = adm1_border_color)
          }
        }
        if (self$target_adm_level == 3) {
          if (!is.null(self$adm1_shapefile)) {
            map <- map +
              tmap::tm_shape(adm1_shapefile) +
              tmap::tm_borders(lwd = adm1_border_thickness, col = adm1_border_color)
          }
          if (!is.null(self$adm2_shapefile)) {
            map <- map +
              tmap::tm_shape(adm2_shapefile) +
              tmap::tm_borders(lwd = adm2_border_thickness, col = adm2_border_color)
          }
        }
        print(map)
      }
    }
  )
)

# Rainfall ----------------------------------------------------------------
#' @export
Rainfall <- R6::R6Class(
  # nolint
  classname = "Rainfall",
  inherit = RasterResource,
  public = list(
    global_api = "https://data.chc.ucsb.edu/products/CHIRPS-2.0/global_monthly/tifs/",
    download = function(target,
                        path_to_save,
                        start_date,
                        end_date) {
    },
    clean = function() {
      self$data <-
        self$data |>
        dplyr::mutate(
          year = stringr::str_extract(file, "\\d{4}"),
          month = stringr::str_extract(file, "\\d{2}(?=.tif)")
        )
    }
  )
)

# Plasmodium Index --------------------------------------------------------

MAPPlasmodiumIndex <- R6::R6Class(
  "MAPPlasmodiumIndex",
  inherit = RasterResource,
  public = list(
    api_url = NULL,
    is_online = NULL,
    is_batch = NULL,
    local_file_type = NULL,
    with_95CI = NULL,
    download_to = NULL,
    plasmodium_index = NULL,
    initialize = function(adm0_shapefile = NULL,
                          adm1_shapefile = NULL,
                          adm2_shapefile = NULL,
                          adm3_shapefile = NULL,
                          with_95CI = TRUE,
                          is_online = TRUE,
                          is_batch = FALSE,
                          api_url = NULL,
                          local_file_type = "raster",
                          plasmodium = "pf",
                          index = "prevalence",
                          output_destination = NULL,
                          local_destination = NULL,
                          download_to = NULL,
                          ...) {
      # TODO: change 2022 to the current year
      if (plasmodium == "pf") {
        plasmodium <- "Pf"
      } else if (plasmodium == "pv") {
        plasmodium <- "Pv"
      } else {
        stop("Plasmodium should be pf or pv")
      }
      if (index == "prevalence") {
        self$plasmodium_index <- paste0(plasmodium, "PR")
        plasmodium_index_local_destination <-
          paste0(plasmodium, "PR")
      } else if (index == "incidence") {
        self$plasmodium_index <- paste0(plasmodium, "IC")
        plasmodium_index_local_destination <-
          paste0(plasmodium, "_", index, "_rate")
      } else if (index == "mortality") {
        self$plasmodium_index <- paste0(plasmodium, "MT")
        plasmodium_index_local_destination <-
          paste0(plasmodium, "_", index, "_rate")
      } else {
        stop("Index should be prevalence, incidence or mortality")
      }
      if (is.null(local_destination)) {
        if (with_95CI) {
          local_destination <- list(
            paste0(
              "Global/Data/MAP/2022_GBD_",
              self$plasmodium_index,
              "_estimates/Raster Data/",
              plasmodium_index_local_destination,
              "_lci"
            ),
            paste0(
              "Global/Data/MAP/2022_GBD_",
              self$plasmodium_index,
              "_estimates/Raster Data/",
              plasmodium_index_local_destination,
              "_rmean"
            ),
            paste0(
              "Global/Data/MAP/2022_GBD_",
              self$plasmodium_index,
              "_estimates/Raster Data/",
              plasmodium_index_local_destination,
              "_uci"
            )
          )
        } else {
          local_destination <-
            paste0(
              "Global/Data/MAP/2022_GBD_",
              self$plasmodium_index,
              "_estimates/Raster Data/",
              plasmodium_index_local_destination,
              "_rmean"
            )
        }
      }
      # TODO change the year 2020 to the current year
      if (is.null(output_destination)) {
        output_destination <- file.path(
          "Countries",
          snt_country,
          "2020_SNT",
          "Analysis",
          "output",
          self$plasmodium_index
        )
      }
      if (is.null(download_to)) {
        download_to <- file.path(
          "Global",
          "Data",
          "MAP",
          "2022_GBD_",
          self$plasmodium_index,
          "_estimates"
        )
      }

      super$initialize(
        adm0_shapefile = adm0_shapefile,
        adm1_shapefile = adm1_shapefile,
        adm2_shapefile = adm2_shapefile,
        adm3_shapefile = adm3_shapefile,
        with_95CI = with_95CI,
        is_online = is_online,
        is_batch = is_batch,
        api_url = api_url,
        local_file_type = local_file_type,
        output_destination = output_destination,
        local_destination = local_destination,
        download_to = download_to,
        index_name = self$plasmodium_index,
        ...
      )

      self$api_url <-
        paste0(
          "https://malariaatlas.org/wp-content/uploads/2022-gbd2020/",
          self$plasmodium_index,
          ".zip"
        )
      self$with_95CI <- with_95CI
      invisible(self)
    },
    read_csv = function(from = NULL, filename) {
      self$data <- readr::read_csv(file = file.path(
        self$output_destination,
        paste(self$plasmodium_index,
          snt_country,
          ".csv",
          sep = "_"
        )
      ))
    },
    clean = function() {
      index <- self$plasmodium_index
      if (self$with_95CI) {
        self$data[[1]] <- self$data[[1]] |> rename(lci = value)
        self$data[[2]] <- self$data[[2]] |> rename(!!index := value)
        self$data[[3]] <- self$data[[3]] |> rename(uci = value)
        self$data[[1]][[index]] <- self$data[[2]][[index]]
        self$data[[1]]$uci <- self$data[[3]]$uci
        self$data <- self$data[[1]]
      } else {
        self$data <- dplyr::rename(self$data, !!index := value)
      }
      self$data <-
        self$data |> dplyr::mutate(
          year = stringr::str_extract(
            file, "\\d+(?=\\.\\w+$)"
          )
        )
    },
    export = function(to = NULL, filename = NULL) {
      if (is.null(to)) {
        to <- self$output_destination
      }
      if (is.null(filename)) {
        filename <- paste0(
          "MAP_",
          snt_country,
          "_",
          self$plasmodium_index,
          ".csv"
        )
      }
      write.csv(self$data,
        file = file.path(to, filename)
      )
      invisible(self)
    },
    plot_line = function(save = TRUE) {
      # if with uncertainty
      # self$data will be a list
      facet_by <- paste0("adm", self$target_adm_level)
      if (self$with_95CI) {
        ggplot2::ggplot(self$data) +
          ggplot2::geom_line(ggplot2::aes(
            y = !!self$plasmodium_index,
            x = year,
            group = !!facet_by
          )) +
          ggplot2::geom_ribbon(
            ggplot2::aes(
              ymin = lci,
              ymax = uci,
              y = !!self$plasmodium_index,
              x = year,
              group = !!facet_by
            ),
            alpha = 0.2
          ) +
          ggplot2::labs(y = self$plasmodium_index, title = snt_country) +
          ggplot2::facet_wrap(as.formula(paste("~", facet_by)), ncol = 5)
      } else {
        ggplot2::ggplot(self$data) +
          ggplot2::geom_line(ggplot2::aes(
            y = !!self$plasmodium_index,
            x = year,
            group = !!facet_by
          )) +
          ggplot2::labs(y = self$plasmodium_index, title = snt_country) +
          ggplot2::facet_wrap(as.formula(paste("~", facet_by)), ncol = 5)
      }
    },
    plot_map = function(year = NULL,
                        palette = "YlGn",
                        reverse_color_order = FALSE,
                        categories = 5,
                        breaks = NULL,
                        label = TRUE,
                        adm1_border_thickness = 5,
                        adm2_border_thickness = 3,
                        adm3_border_thickness = 1,
                        adm1_name_in_shp = NULL,
                        adm2_name_in_shp = NULL,
                        adm3_name_in_shp = NULL,
                        title = NULL) {
      # merge shape file with data
      if (!is.null(self$adm1_shapefile)) {
        adm1_shapefile <- sf::st_read(self$adm1_shapefile)
        dplyr::rename(adm1_shapefile, adm1 = !!adm1_name_in_shp)
      }
      if (!is.null(self$adm2_shapefile)) {
        adm2_shapefile <- sf::st_read(self$adm2_shapefile)
        adm2_shapefile <- adm2_shapefile |>
          dplyr::rename(adm2 = !!adm2_name_in_shp)
      }
      if (!is.null(self$adm3_shapefile)) {
        adm3_shapefile <- sf::st_read(self$adm3_shapefile)
        adm3_shapefile <- adm3_shapefile |>
          dplyr::rename(adm3 = !!adm3_name_in_shp)
      }
      my_data <- self$data

      if (length(palette) == 1) {
        # if palette is "foo"
        palette <- rev(grDevices::hcl.colors(
          categories,
          palette
        ))
      } else if (length(palette) > 1) {
        # if palette is c('foo','bar')
        palette <- palette
      }

      if (missing(year)) {
        # Join map and data
        map_and_data <- switch(self$target_adm_level,
          "1" = dplyr::inner_join(adm1_shapefile, my_data),
          "2" = dplyr::inner_join(adm2_shapefile, my_data),
          "3" = dplyr::inner_join(adm3_shapefile, my_data)
        )
        # dplyr::inner_join(palette.colors(), my_data)
        map <- tmap::tm_shape(map_and_data) +
          tmap::tm_borders() +
          tmap::tm_fill(
            self$plasmodium_index,
            n = categories,
            palette = palette,
            breaks = breaks
          ) +
          tmap::tm_facets(by = "year") +
          tmap::tm_layout(title = snt_country)

        if (self$target_adm_level == 2) {
          if (!is.null(self$adm1_shapefile)) {
            map <- map +
              tmap::tm_shape(adm1_shapefile) +
              tmap::tm_borders(lwd = adm1_border_thickness)
          }
        }
        if (self$target_adm_level == 3) {
          if (!is.null(self$adm1_shapefile)) {
            map <- map +
              tmap::tm_shape(adm1_shapefile) +
              tmap::tm_borders(lwd = adm1_border_thickness)
          }
          if (!is.null(self$adm2_shapefile)) {
            map <- map +
              tmap::tm_shape(adm2_shapefile) +
              tmap::tm_borders(lwd = adm2_border_thickness)
          }
        }
        print(map)
      } else {
        my_data <- dplyr::filter(my_data, year == !!year)
        map_and_data <- switch(self$target_adm_level,
          "1" = dplyr::inner_join(adm1_shapefile, my_data),
          "2" = dplyr::inner_join(adm2_shapefile, my_data),
          "3" = dplyr::inner_join(adm3_shapefile, my_data)
        )
        # plot specified year
        map <- tmap::tm_shape(map_and_data) +
          tmap::tm_borders() +
          tmap::tm_fill(
            self$plasmodium_index,
            n = categories,
            palette = palette,
            breaks = breaks
          )

        # if title is false, display no title
        # if title is true, display default title
        # if title is string, display the preset title
        if (is.logical(title)) {
          if (title == TRUE) {
            map <- map + tmap::tm_layout(title = paste(snt_country, year,
              sep = "-"
            ))
          }
        } else if (is.character(title)) {
          map <- map + tmap::tm_layout(title = title)
        }

        if (self$target_adm_level == 2) {
          if (!is.null(self$adm1_shapefile)) {
            map <- map +
              tmap::tm_shape(adm1_shapefile) +
              tmap::tm_borders(lwd = adm1_border_thickness)
          }
        }
        if (self$target_adm_level == 3) {
          if (!is.null(self$adm1_shapefile)) {
            map <- map +
              tmap::tm_shape(adm1_shapefile) +
              tmap::tm_borders(lwd = adm1_border_thickness)
          }
          if (!is.null(self$adm2_shapefile)) {
            map <- map +
              tmap::tm_shape(adm2_shapefile) +
              tmap::tm_borders(lwd = adm2_border_thickness)
          }
        }
        print(map)
      }
    }
  )
)

## Pf Prevalence -----------------------------------------------------------
# TODO implement without 95 CI function
#' @export
PfPrevalence <- R6::R6Class(
  "PfPrevalence",
  inherit = MAPPlasmodiumIndex,
  public = list(
    api_url = NULL,
    is_online = NULL,
    is_batch = NULL,
    local_file_type = NULL,
    with_95CI = NULL,
    download_to = NULL,
    initialize = function(adm1_shapefile = NULL,
                          adm2_shapefile = NULL,
                          adm3_shapefile = NULL,
                          with_95CI = TRUE,
                          is_online = TRUE,
                          is_batch = FALSE,
                          api_url = NULL,
                          local_file_type = "raster",
                          plasmodium = "pf",
                          index = "prevalence",
                          output_destination = NULL,
                          local_destination = NULL,
                          download_to = NULL,
                          ...) {
      super$initialize(
        adm1_shapefile = adm1_shapefile,
        adm2_shapefile = adm2_shapefile,
        adm3_shapefile = adm3_shapefile,
        with_95CI = with_95CI,
        is_online = is_online,
        is_batch = is_batch,
        api_url = api_url,
        local_file_type = local_file_type,
        plasmodium = plasmodium,
        index = index,
        output_destination = output_destination,
        local_destination = local_destination,
        download_to = download_to,
        ...
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
      # The sequence of 95 CI with mean will be LCI, rMean, UCI

      return(long)
    }
  )
)

#' @export
PfIncidence <- R6::R6Class(
  "PfIncidence",
  inherit = MAPPlasmodiumIndex,
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
        long$year <- as.numeric(substr(long$name, 34, 37))
      } else if (index == 2) {
        colnames(long)[1] <- "MEAN"
        long$year <- as.numeric(substr(long$name, 39, 42))
      }
      return(long)
    }
  )
)

# TODO implement without 95 CI function
#' @export
PfMortality <- R6::R6Class(
  "PfMortality",
  inherit = MAPPlasmodiumIndex,
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
        long$year <- as.numeric(substr(long$name, 34, 37))
      } else if (index == 2) {
        colnames(long)[1] <- "MEAN"
        long$year <- as.numeric(substr(long$name, 39, 42))
      }
      return(long)
    }
  )
)

# TODO implement without 95 CI function
#' @export
PvPrevalence <- R6::R6Class(
  "PVPrevalence",
  inherit = MAPPlasmodiumIndex,
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
  inherit = MAPPlasmodiumIndex,
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
        # TODO change this after download file and check the folder structure
        # if LCI or UCI
        # example file naming will be
        # PvPR_UCI_Global_admin0_2000.tif
        # PvPR_LCI_Global_admin0_2000.tif
        colnames(long)[1] <- substr(long$name, 16, 18)
        long$year <- as.numeric(substr(long$name, 34, 37))
      } else if (index == 2) {
        colnames(long)[1] <- "MEAN"
        long$year <- as.numeric(substr(long$name, 39, 42))
      }
      return(long)
    }
  )
)

# IHME Mortality --------------------------------------------------------

IHME_mortality <- R6::R6Class(
  "IHME_mortality",
  inherit = RasterResource,
  public = list(
    api_url = NULL,
    is_online = NULL,
    is_batch = NULL,
    local_file_type = NULL,
    with_95CI = NULL,
    download_to = NULL,
    index_name = NULL,
    initialize = function(adm1_shapefile = NULL,
                          adm2_shapefile = NULL,
                          adm3_shapefile = NULL,
                          is_online = TRUE,
                          is_batch = FALSE,
                          api_url = NULL,
                          local_file_type = "raster",
                          with_95CI = FALSE,
                          output_destination = NULL,
                          local_destination = NULL,
                          download_to = NULL,
                          index_name = "au5mr_cat",
                          ...) {
      super$initialize(
        adm1_shapefile = adm1_shapefile,
        adm2_shapefile = adm2_shapefile,
        adm3_shapefile = adm3_shapefile,
        with_95CI = with_95CI,
        is_online = is_online,
        is_batch = is_batch,
        api_url = api_url,
        local_file_type = local_file_type,
        output_destination = output_destination,
        local_destination = local_destination,
        download_to = download_to,
        index_name = index_name,
        ...
      )
      invisible(self)
    },
    read_csv = function(from = NULL, filename) {
      if (is.null(from)) {
        from <- self$output_destination
      }
      if (is.null(filename)) {
        filename <- paste0(
          "IHME_",
          snt_country,
          "_",
          self$index_name,
          ".csv"
        )
      }
      self$data <- read.csv(file = file.path(from, filename))
    },
    clean = function() {
      index <- self$index_name
      if (self$with_95CI) {
        self$data[[1]] <- self$data[[1]] |> rename(lci = value)
        self$data[[2]] <- self$data[[2]] |> rename(!!index := value)
        self$data[[3]] <- self$data[[3]] |> rename(uci = value)
        self$data[[1]][[index]] <- self$data[[2]][[index]]
        self$data[[1]]$uci <- self$data[[3]]$uci
        self$data <- self$data[[1]]
      } else {
        self$data <- dplyr::rename(self$data, !!index := value)
      }
      self$data <-
        self$data |>
        dplyr::mutate(
          year = stringr::str_extract(file, "(\\d{4})(?=_Y2017M09D25)")
        )
    },
    export = function(to = NULL, filename = NULL) {
      if (is.null(to)) {
        to <- self$output_destination
      }
      if (is.null(filename)) {
        filename <- paste0(
          "IHME_",
          snt_country,
          "_",
          self$index_name,
          ".csv"
        )
      }
      write.csv(self$data,
        file = file.path(to, filename)
      )
      invisible(self)
    }
  )
)


# Utils -------------------------------------------------------------------
#' @export
fix_outliers <- function(data, outliers_df) {

}

#' @export
outliers_find_hf <- function(outliers,
                             group_by_column = "hf") {
  result <-
    outliers |>
    dplyr::group_by(!!group_by_column) |>
    dplyr::summarise(count = dplyr::n()) |>
    dplyr::arrange(dplyr::desc(dplyr::count))
  return(result)
}
