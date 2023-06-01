#' @title Generate Default Config
#' @description Write config.yml if not exists
#' @export
#' @seealso https://github.com/johnaponte/repana
#' @param country country name
#' @param db database type, either 'postgres' or 'SQLite'
#' @examples
#' generate_default_config(country = 'nigeria')
#' generate_default_config(country = 'nigeria', db = 'postgres')
#' generate_default_config(country = 'nigeria', db = 'SQLite')
generate_default_config <- function(country = "Nigeria", db = "SQLite") {
  country <- paste0(" country: ", country, "\n")
  cores <- config_parallel()
  db <- config_set_db(db)
  if (!file.exists("config.yml")) {
    cat(
      "default:\n", country, " dirs:\n", "   data: _data\n",
      "   functions: _functions\n", "   handmade: handmade\n",
      "   database: database\n", "   reports: reports\n",
      "   logs: logs\n", " clean_before_new_analysis:\n",
      "   - database\n", "   - reports\n", "   - logs\n",
      db, cores, file = "config.yml"
    )
  }
}
#' @title Generate Default .gitignore
#' @description Write .gitignore if not exists
#' @export
#' @seealso https://github.com/johnaponte/repana
#' @importFrom config get
#' @keywords git, gitignore, file
generate_default_gitignore <- function() {
  # Process the .gitignore file
  if (!file.exists(".gitignore")) {
    cat("# Created by make_structure\n", file = ".gitignore")
  }
  xx <- trimws(
    readLines(".gitignore"),
    "both"
  )
  if (get_os() == "osx" && !any(grep("^\\.DS_Store$", xx))) {
    cat(".DS_Store\n", file = ".gitignore", append = TRUE)
  }
  if (!any(
    grepl(
      paste0("^config.yml$"),
      xx
    )
  )) {
    cat("config.yml", "\n", file = ".gitignore", append = TRUE)
  }
  in_gitignore <- config::get("clean_before_new_analysis")
  lapply(
    in_gitignore, function(x) {
      tdir <- trimws(
        config::get("dirs")[[x]],
        "both"
      )
      if (length(tdir) ==
        0) {
        stop(x, " not defined in dirs. check the config.yml file\n")
      }
      if (!any(
        grepl(
          paste0("^", tdir, "$"),
          xx
        )
      )) {
        cat(tdir, "\n", file = ".gitignore", append = TRUE)
      }
    }
  )
}
#' @title Make Structure
#' @description Make the structure of the project
#' @export
setup <- function() {
  generate_default_config()
  generate_default_gitignore()
  mkdir()
}
#' Get path
#'
#' Get path from config.yml and from user defined file
#' @param type type of path, could be 'data',
#' @param path path of the file
#' @export
#' @return return the path
#' @examples get_path('data', 'test.csv')
get_path <- function(type, path) {
  config <- config::get()
  type <- config[["dirs"]][[type]]
  return(file.path(type, path))
}
#' @title Check type of path
#' @description check type of path. check the existence of the dir or file
#' if they exist then return the type of the path
#' else return false
#' @param path String path
#' @export
type_of_path <- function(path) {
  # If the path is not a character string, return an error
  # if (!is.character(path)) { stop('Error: path must be a
  # character string') }
  # # If the path is an empty string, return an error if
  # (nchar(path) == 0) { stop('Error: path cannot be an
  # empty string') }
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
# Database ------
#' Config database
#'
#' @param db_name name of database, example malaria
#' @param db_user username of the database, for connection
#' @param db_pass password of the database, for connection
#'
#' @return a list contain the db info
#' @rdname config
config_set_db <- function(
  db = "SQLite", db_name = NULL, db_user = NULL, db_pass = NULL
) {
  if (db == "postgres") {
    db <- paste0(
      " db:\n", "   package: RPostgres\n", "   dbconnect: Postgres\n",
      "   host: localhost\n", "   port: 5432\n", "   name: malaria\n",
      "   user: postgres\n", "   password: postgres\n"
    )
  } else if (db == "SQLite") {
    db <- paste0(
      " db:\n", "   package: RSQLite\n", "   dbconnect: SQLite\n",
      "   dbname: \":memory:\"\n"
    )
  } else {
    stop("db must be either 'postgres' or 'SQLite'")
  }
  return(db)
}
#' Get Db Connection from Configurations
#'
#' @return db connection
#' @export
#' @importFrom config get
#' @rdname config
config_get_db <- function() {
  host <- get("db")$host
  port <- get("db")$port
  name <- get("db")$name
  user <- get("db")$user
  pass <- get("db")$pass
  return(
    list(
      host = host, port = port, name = name, user = user,
      pass = pass
    )
  )
}
# Parallel -----
#' Configuration for parallel execution
#'
#' @return a list contain the parallel execution info
#' @export
#' @rdname config
config_parallel <- function() {
  cores <- parallel::detectCores()
  return(paste0(" cores: ", cores, "\n"))
}
# Country -----
#' @title Get Country from the yaml configuration file
#' @description Get the country from the yaml configuration file
#' TODO: format country name to ISO3 country code
#' @return country
#' @export
#' @importFrom config get
#' @examples config_get_country()
#' @rdname config
config_get_country <- function() {
  country <- get("country")
  return(country)
}
