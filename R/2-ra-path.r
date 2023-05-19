#' @title Get OS
#' @description Get Operating System from R
#' thanks to will at  https://www.r-bloggers.com/identifying-the-os-from-r/
#' @return string of operating system
#' @export
get_os <- function() {
  sysinf <- Sys.info()
  if (!is.null(sysinf)) {
    os <- sysinf["sysname"]
    if (os == "Darwin") {
      os <- "osx"
    }
  } else {
    ## mystery machine
    os <- .Platform$OS.type
    if (grepl("^darwin", R.version$os)) {
      os <- "osx"
    }
    if (grepl("linux-gnu", R.version$os)) {
      os <- "linux"
    }
  }
  tolower(os)
  return(os)
}

#' @title Make Directory
#' @description Make Directory based on the config
#' @return NULL
#' @export
#' @importFrom config get
mkdir <- function() {
  lapply(
    c(get("dirs")),
    function(x) {
      if (!dir.exists(x)) {
        dir.create(x, recursive = TRUE)
      }
    }
  )
}
