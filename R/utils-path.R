#' @title Check type of path
#' @description check type of path. check the existence of the dir or file
#' if they exist then return the type of the path
#' else return false
#' @param path String path
type_of_path <- function(path) {
    result <- NULL
      # If the path is not a character string, return an error
      if (!is.character(path)) {
          stop("Error: path must be a character string")
      }

      # If the path is an empty string, return an error
      if (nchar(path) == 0) {
          stop("Error: path cannot be an empty string")
      }
    if (dir.exists(path)) {
        result <- "dir"
    } else if (file_test("-f", path)) {
        result <- "file"
    } else {
        result <- FALSE
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
use_relative_or_absolute <- function(root, relative) {
    relative_path_type <- type_of_path(relative)

    if (!isFALSE(relative_path_type)) {
        return(relative)
    }

    combined_root <- file.path(root, relative)
    combined_root_path_type <- type_of_path(combined_root)

    if (!(isFALSE(combined_root_path_type))) {
        return(combined_root)
    }

    return(FALSE)
}

