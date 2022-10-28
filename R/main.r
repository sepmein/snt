# Never use library() or require() in a R package!

#' @export
set_country <- function(root_folder, country) {
  setwd(root_folder)
  snt_country <<- country # nolint
}

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
    #' Resource class
    #' Base class for the sub-national analysis package
    #' @param is_online, Bool, is online resource or not, consider to remote?
    #' if api_url has been set, then is should be an online resource
    #' is_online do have some problems, as all resources do come from
    #'  somewhere online,
    #' but in most cases this process only needed to be done once while,
    #' means the file in not online anymore if this file has already
    #' been downloaded
    #' TODO: should I provide a single download function for all the files
    #' needed
    #' e.g. download_resource(
    #'  resourceA, resourceB
    #' )
    #' Then that's it.?
    #' @param is_batch, Bool, the definition of is_batch is somewhat unclear.
    #' TODO need to be clearer defined. Batch included a few level of batch.
    #' When downloading files, download a single file or a few files
    #' When read from local file
    #' @param api_url
    #' @param local_destination
    #' @param local_file_type
    #' @param output_destination
    #' @param download_to
    #'
    #' @return
    #' @export
    #'
    #' @examples
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

# Rainfall ----------------------------------------------------------------
#' @export
rainfall_api_base <- "ftp://ftp.chg.ucsb.edu/pub/org/chg/products/CHIRPS-2.0/"
Rainfall <- R6::R6Class(
  # nolint
  classname = "Rainfall",
  inherit = Resource,
  public = list(
    africa_api = paste0(rainfall_api_base, "africa_monthly/tifs/"),
    global_api = paste0(rainfall_api_base, "global_monthly/tifs/"),
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
          select_files <-
            self$select_files(
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
        stop(paste0(
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
      ggplot2::ggplot(self$data) +
        ggplot2::geom_line(ggplot2::aes(
          x = date,
          y = rain
        )) +
        ggplot2::facet_wrap(~adm1)
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


# Shapefiles --------------------------------------------------------------
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
    adm1_shapefile = NULL,
    adm2_shapefile = NULL,
    adm3_shapefile = NULL,
    target_adm_level = NULL,
    index_name = NULL,
    initialize = function(adm1_shapefile = NULL,
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
      self$adm1_shapefile <- adm1_shapefile
      self$adm2_shapefile <- adm2_shapefile
      self$adm3_shapefile <- adm3_shapefile
    },
    load = function(target_adm_level = 2,
                    adm1_name_in_shp = NULL,
                    adm2_name_in_shp = NULL,
                    adm3_name_in_shp = NULL,
                    method = "mean") {
      self$target_adm_level <- target_adm_level
      # set target shapefile based on target level
      if (target_adm_level == 1) {
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
      loaded_shp <- rgdal::readOGR(target_shapefile)
      if (typeof(self$local_destination) == "list") {
        self$data <- self$load_multiple_folder(
          target_adm_level,
          adm1_name_in_shp,
          adm2_name_in_shp,
          adm3_name_in_shp,
          method,
          loaded_shp
        )
      } else if (dir.exists(self$local_destination)) {
        self$data <- self$load_single_folder(
          target_adm_level,
          adm1_name_in_shp,
          adm2_name_in_shp,
          adm3_name_in_shp,
          method,
          loaded_shp
        )
      } else if (file_test("-f", self$local_destination)) {
        self$data <- self$load_single_file(
          target_adm_level,
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
      invisible(self)
    },
    clean = function() {

    },
    load_single_file = function(target_adm_level,
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

      # set adm1, adm2, adm3 from shp
      if (target_adm_level == 1) {
        result$adm1 <- loaded_shp[[adm1_name_in_shp]]
      } else if (target_adm_level == 2) {
        if (!is.null(adm1_name_in_shp)) {
          result$adm1 <-
            loaded_shp[[adm1_name_in_shp]]
        }
        result$adm2 <- loaded_shp[[adm2_name_in_shp]]
      } else if (target_adm_level == 3) {
        if (!is.null(adm1_name_in_shp)) {
          result$adm1 <-
            loaded_shp[[adm1_name_in_shp]]
        }
        if (!is.null(adm2_name_in_shp)) {
          result$adm2 <-
            loaded_shp[[adm2_name_in_shp]]
        }
        result$adm3 <- loaded_shp[[adm3_name_in_shp]]
      }

      # return result
      return(result)
    },
    get_district_raster_data = function(method,
                                        extracted_raster_data) {
      if (method == "mean") {
        result <- lapply(extracted_raster_data,
          FUN = mean, na.rm = TRUE
        )
      }
      return(result)
    },
    load_single_folder = function(target_adm_level,
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
      result <- NULL
      # Extract raster values to list object
      for (j in seq_along(raster_files)) {
        the_file <- file.path(
          folder,
          raster_files[j]
        )
        loaded <- self$load_single_file(
          target_adm_level,
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
                                    adm1_name_in_shp,
                                    adm2_name_in_shp,
                                    adm3_name_in_shp,
                                    method,
                                    loaded_shp) {
      result <- list()
      for (i in seq_along(self$local_destination)) {
        loaded <- self$load_single_folder(
          target_adm_level,
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
      tmap_options(check.and.fix = TRUE)

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
              tmap::tm_borders(
                lwd = adm1_border_thickness,
                col = adm1_border_color
              )
          }
        }
        if (self$target_adm_level == 3) {
          if (!is.null(self$adm1_shapefile)) {
            map <- map +
              tmap::tm_shape(adm1_shapefile) +
              tmap::tm_borders(
                lwd = adm1_border_thickness,
                col = adm1_border_color
              )
          }
          if (!is.null(self$adm2_shapefile)) {
            map <- map +
              tmap::tm_shape(adm2_shapefile) +
              tmap::tm_borders(
                lwd = adm2_border_thickness,
                col = adm2_border_color
              )
          }
        }
        print(map)
      }
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
      self$data <- read.csv(file = file.path(
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
        self$data |>
        dplyr::mutate(
          year = stringr::str_extract(
            file,
            "\\d+(?=\\.\\w+$)"
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
        # if title is string, display the settled title
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
          year = stringr::str_extract(
            file,
            "(\\d{4})(?=_Y2017M09D25)"
          )
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
smart_get_file_list_by_year <- function(smart_path) {
  # detect country code
  # find special character in special path parameter
  # loop through file path in the path.list
  # return valid file
  pattern <- "(\\*yyyy\\*)"
  detected_pattern <- stringr::str_detect(smart_path, pattern)
  if (!(detected_pattern)) {
    stop("path do not contain pattern, please modify the year into *yyyy*")
  }
  target_years <- 1980:2030
  result_file_list <- c()
  result_years <- c()
  for (i in seq_along(target_years)) {
    replacement_year <- as.character(target_years[i])
    target_file_path <- file.path(stringr::str_replace(
      smart_path,
      pattern, replacement_year
    ))
    if (file.exists(target_file_path)) {
      result_file_list <- append(result_file_list, target_file_path)
      result_years <- append(result_years, target_years[i])
    }
  }
  return(list(
    files = result_file_list,
    years = result_years
  ))
}

#' @export
smart_read_excel_by_year <-
  function(reader,
           smart_path,
           skip = 0,
           clean = TRUE,
           country = snt_country) {
    if (is.null(snt_country)) { # nolint
      warning(paste0(
        "snt::set_country method has not run yet,",
        " this will cause problem."
      ))
    }
    file_list <-
      reader(smart_path)
    data_tables <-
      purrr::map(
        file_list$files,
        ~ readxl::read_excel(.x, skip = skip)
      )
    # create a nested tibble
    result <- tibble::tibble(
      year = file_list$years,
      data = data_tables
    )
    if (clean) {
      result <- result |>
        # rename using internal rename database
        dplyr::mutate(
          data = purrr::map(
            data,
            ~ routine_rename(.x, country = country)
          )
        ) |>
        # replace using internal replace database
        dplyr::mutate(
          data = purrr::map(
            data,
            ~ routine_replace(.x, country = country)
          )
        )
    }
    return(result)
  }

#' @export
smart_get_all_files_in_dir <-
  function(smart_path,
           skip = 0,
           clean = TRUE,
           country = snt_country) {
    if (!(dir.exists(smart_path))) {
      stop("Path does not exists.")
    }
    file_list <- list.files(smart_path)
    # remove temp xlsx file "~$Bo District.xlsx"
    file_list <- purrr::keep(
      file_list,
      ~ !stringr::str_starts(.x, "~")
    )
    # map get full path
    file_list <- file.path(smart_path, file_list)
    data_tables <-
      purrr::map(
        file_list,
        ~ readxl::read_excel(.x, skip = skip)
      )
    # create a nested tibble
    result <- tibble::tibble(
      file = file_list,
      data = data_tables
    )
    if (clean) {
      result <- result |>
        # rename using internal rename database
        dplyr::mutate(
          data = purrr::map(
            data,
            ~ routine_rename(.x, cty = country)
          )
        ) |>
        # replace using internal replace database
        dplyr::mutate(
          data = purrr::map(
            data,
            ~ routine_replace(.x, cty = country)
          )
        )
    }
    return(result)
  }


#' @export
update_database <- function() {
  import_routine_rename <-
    readr::read_csv("inst//extdata//routine//01_rename.csv")
  usethis::use_data(import_routine_rename, overwrite = TRUE)
  import_routine_replace <-
    readr::read_csv("inst//extdata//routine//02_replace.csv")
  usethis::use_data(import_routine_replace, overwrite = TRUE)
  import_routine_replace_hfname <-
    readr::read_csv("inst//extdata//routine//03_replace_hfname.csv")
  usethis::use_data(import_routine_replace_hfname, overwrite = TRUE)
  import_routine_set_cluster <-
    readr::read_csv("inst//extdata//routine//04_cluster.csv")
  usethis::use_data(import_routine_set_cluster, overwrite = TRUE)
  devtools::load_all()
}

#' @export
find_outlier <-
  function(df,
           columns = c(
             "susp",
             "test_rdt",
             "test_mic",
             "test_rdt_lab",
             "test",
             "abn_mic",
             "abn_rdt",
             "conf_rdt",
             "maltreat_u5",
             "maltreat_ov5",
             "maltreat",
             "maldth"
           ),
           alpha = 0.999,
           both_sides = FALSE,
           select_rows = c(
             "ID",
             "adm1",
             "adm2",
             "adm3",
             "hfca",
             "hfname",
             "hf",
             "year",
             "month",
             "yearmon",
             "index",
             "value"
           )) {
    result <- tibble::tibble(
      ID = integer(),
      value = numeric(),
      index = character()
    )
    for (column in columns) {
      ### algorithm upper bound
      upper_bound <- quantile(df[[column]], alpha, na.rm = TRUE)
      outlier_index <-
        which(df[[column]] > upper_bound)
      df_outlier_list <- df[outlier_index, ] |>
        # select id and target column
        dplyr::select(dplyr::one_of(c("ID", column))) |>
        dplyr::mutate(index = !!column) |>
        dplyr::rename(value = !!column)
      result <- result |> dplyr::full_join(df_outlier_list)
    }
    result <- result |>
      dplyr::left_join(df) |>
      dplyr::select(!!select_rows)
    return(result)
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
