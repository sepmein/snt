#' @title Check type of path
#' @description check type of path. check the existence of the dir or file
#' if they exist then return the type of the path
#' else return false
#' @param path String path
#' @export
type_of_path <- function(path) {
    # If the path is not a character string, return an error
    # if (!is.character(path)) {
    #     stop("Error: path must be a character string")
    # }

    # # If the path is an empty string, return an error
    # if (nchar(path) == 0) {
    #     stop("Error: path cannot be an empty string")
    # }
    if (dir.exists(path)) {
        result <- "dir"
    } else if (file_test("-f", path)) {
        result <- "file"
    } else {
        result <- NULL
    }
    return(result)
}

#' @title Check to use relative or absolute path
#' @description Combine root and relative check if they exist in path
#' Check relative path exists
#' if it is true then use the combined path
#' if it is false then use the relative path
#'
#' @param root String, root path
#' @param relative String, relative path
#' @return path
#' @export
use_relative_or_absolute <- function(root, relative) {
    relative_path_type <- type_of_path(relative)

    if (!is.null(relative_path_type)) {
        return(relative)
    }

    combined_root <- file.path(root, relative)
    combined_root_path_type <- type_of_path(combined_root)

    if (!is.null(combined_root_path_type)) {
        return(combined_root)
    }

    return(FALSE)
}

#' Central Configuration for SNT analysis
#'
#' @param country ISO3 code for a country, e.g. nigeria should be NGA
#' @param root path of the root file, could be NULL
#' if set, will be combined with other root folders
#' @param root_input path of the input root, could be NULL
#' @param root_output path of the output root. If set, will be combined with
#'
#' @param root_raster
#' @param map_folder
#' @param ihme_folder
#' @param rainfall_folder
#' @param rainfall_output
#' @param shapefile
#'
#' @return
#' @export
#'
#' @examples
#' config(
#'     country = "NGA",
#'     root = "/Users/sepmein/snt/NGA",
#'     root_input = "input",
#'     root_output =
#'     )
#' @description Central Configuration settings for SNT analysis.
#' This function could set the following configurations:
#' 1. the target subnational analysis country
#' 2. folder structure
#'  All the folder structure could be set separately,
#' however it will be nice if they are all under one root folder.
#'  root
#'  | --- output the status of data
#'  | --- input
#'        | --- 
#'  | --- graph the type of output
#'  | --- report
#'  | --- raster 
#' 3. postgresql db connection
config <- function(country,
                   root = NULL,
                   root_input = NULL,
                   root_output = NULL,
                   path_raster = NULL,
                   path_raster_map = NULL,
                   path_raster_ihme = NULL,
                   path_raster_rainfall = NULL,
                   path_country = NULL,
                   path_routine = NULL,
                   path_intervention = NULL,
                   path_dhs = NULL,
                   db_name = NULL,
                   db_user = NULL,
                   db_pass = NULL,
                   path_shapefile = NULL) {
    # TODO add a re-format function to format country to ISO3 code

    result <- list(
        "country" = country,
        "data_folder" = config_data_folder(
            root,
            root_input,
            path_raster,
            path_raster_map,
            path_raster_ihme,
            path_raster_rainfall,
            path_country,
            path_routine,
            path_intervention,
            path_dhs,
            root_output
        ),
        "parallel" = config_parallel(),
        "shapefile" = config_shapefile(root, path_shapefile),
        "db" = config_db(
            db_name = db_name,
            db_user = db_user,
            db_pass = db_pass
        )
    )
    return(result)
}

#' @title config data folder
#' @description config data folder for SNT analysis
#' @param root path of the root file, could be null
#' @param root_input path of the input root, could be null
#' @param path_raster path of the raster root, could be null
#' @param path_raster_map path of the MAP estimation, could not be null
#' @param path_raster_ihme path of the IHME estimation, could not be null
#' @param path_rainfall path of the rainfall data, could not be null
#' @param path_routine path of the routine root, contains the routine data
#' @param path_intervention path of the intervention root,
#' contains the intervention data
#' @return a list of data folder
config_data_folder <- function(root = NULL,
                               root_input,
                               path_raster,
                               path_raster_map,
                               path_raster_ihme,
                               path_raster_rainfall,
                               path_country,
                               path_routine,
                               path_intervention,
                               path_dhs,
                               root_output) {
    path_input <- use_relative_or_absolute(root, root_input)
    input <- config_input(
        path_input,
        path_raster,
        path_raster_map,
        path_raster_ihme,
        path_raster_rainfall,
        path_country,
        path_routine,
        path_intervention,
        path_dhs
    )
    path_output <- use_relative_or_absolute(root, root_output)
    output <- config_output(
        path_output
    )
    return(list(
        "input" = input,
        "output" = output
    ))
}

#' @title config input folder
#' @description config input folder for SNT analysis
#' @param path_input path of the input root, could be NULL
#' @param path_raster path of the raster root, could be NULL
#' @param path_raster_map path of the MAP estimation
#' @param path_raster_ihme path of the IHME estimation
#' @param path_rainfall path of the rainfall data
#' @param path_country path of the country root, contains the country submitted
#' data
#' @param path_routine path of the routine root, contains the routine data
#' @param path_intervention path of the intervention
#' @param path_dhs path of the DHS data
#' @return a list of input folder
config_input <- function(path_input,
                         path_raster,
                         path_raster_map,
                         path_raster_ihme,
                         path_raster_rainfall,
                         path_country,
                         path_routine,
                         path_intervention,
                         path_dhs) {
    raster <- config_raster(
        use_relative_or_absolute(path_input, path_raster),
        path_raster_map,
        path_raster_ihme,
        path_raster_rainfall
    )
    country <- config_country(
        use_relative_or_absolute(path_input, path_country),
        path_routine,
        path_intervention
    )
    dhs <- config_dhs(
        use_relative_or_absolute(path_input, path_dhs)
    )

    return(
        list(
            "country" = country,
            "raster" = raster,
            "dhs" = dhs
        )
    )
}

#' @title config raster folders
#' @description config raster folders for SNT analysis
#' @param path_raster path of the raster root, could be NULL
#' @param path_raster_map path of the MAP estimation, could not be null
#' @param path_raster_ihme path of the IHME estimation, could not be NULL
#' @param path_raster_rainfall path of the rainfall data, could not be NULL
#' @return a list of raster folders
config_raster <- function(path_raster,
                          path_raster_map,
                          path_raster_ihme,
                          path_raster_rainfall) {
    return(
        list(
            "map" = use_relative_or_absolute(path_raster, path_raster_map),
            "ihme" = use_relative_or_absolute(path_raster, path_raster_ihme),
            "rainfall" = use_relative_or_absolute(
                path_raster, path_raster_rainfall
            )
        )
    )
}

#' @title config country folders
#' @description config country folders for SNT analysis
#' @param path_country path of the country root
#' @param path_routine path of the routine, contains the routine data
#' @param path_intervention path of the intervention
#' contains the intervention data
#' @return a list of country folders
config_country <- function(path_country,
                           path_routine,
                           path_intervention) {
    routine <- use_relative_or_absolute(path_country, path_routine)
    intervention <- use_relative_or_absolute(path_country, path_intervention)
    return(
        list(
            "routine" = routine,
            "intervention" = intervention
        )
    )
}

#' @title config DHS folders
#' @description config DHS folders for SNT analysis
#' @param path_input path of the input root, could be NULL
#' @param path_dhs path of the DHS root
#' @return a list of DHS folders
config_dhs <- function(path_input = NULL,
                       path_dhs = NULL) {
    dhs <- use_relative_or_absolute(path_input, path_dhs)
    return(
        dhs
    )
}

config_output <- function(path_output) {
    return(
        list(
            "incidence" = use_relative_or_absolute(path_output, "incidence"),
            "mortality" = use_relative_or_absolute(path_output, "mortality")
        )
    )
}

config_shapefile <- function(root, shapefile) {
    return(use_relative_or_absolute(root, shapefile))
}

#' Config database
#'
#' @param db_name name of database, example malaria
#' @param db_user username of the database, for connection
#' @param db_pass password of the database, for connection
#'
#' @return a list contain the db info
config_db <- function(db_name = NULL,
                      db_user = NULL,
                      db_pass = NULL) {
    return(list(
        "db_name" = db_name,
        "db_user" = db_user,
        "db_pass" = db_pass
    ))
}

#' Configuration for parallel execution
#'
#' @return a list contain the parallel execution info
config_parallel <- function() {
    cores <- parallel::detectCores()
    return(list("cores" = cores))
}

# TODO
#' @title Setup a folder for analysis
#' @description Provide a path, snt will setup a whole folder settings for you.
#' @param path Path to where you want the folder will be
#' @return NULL
setup <- function(path) {

}
