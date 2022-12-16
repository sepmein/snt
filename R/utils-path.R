#' @title Check type of path
#' @description check type of path. check the existence of the dir or file
#' if they exist then return the type of the path
#' else return false
#' @param path String path
type_of_path <- function(path) {
    result <- NULL
    if (dir.exists(path)) {
        result <- "dir"
    } else if (file_test("-f", path)) {
        result <- "file"
    } else {
        result <- FALSE
    }
    return(result)
}