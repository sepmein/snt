#' Devtools : Update SNT databases
#'
#' @return NULL
#' @export
#'
#' @examples
#' # edit db in inst/extdata, then run
#' update_database()
#'
#' @description When importing data, snt often requires a lot of renaming
#' those db were created for reusable renaming process
#'
#' @import usethis
#' @import readr
#' @importFrom devtools document
#' @importFrom devtools load_all
update_database <- function() {
  import_routine_rename <- readr::read_csv("inst//extdata//routine//01_rename.csv")
  usethis::use_data(import_routine_rename, overwrite = TRUE)
  import_routine_adm1_replace <- readr::read_csv("inst//extdata//routine//01_adm1_replace.csv")
  usethis::use_data(import_routine_adm1_replace, overwrite = TRUE)
  import_routine_adm2_replace <- readr::read_csv("inst//extdata//routine//01_adm2_replace.csv")
  usethis::use_data(import_routine_adm2_replace, overwrite = TRUE)
  import_routine_adm2_replace_by_adm1 <- readr::read_csv("inst//extdata//routine//01_adm2_replace_by_adm1.csv")
  usethis::use_data(import_routine_adm2_replace_by_adm1, overwrite = TRUE)
  import_routine_replace <- readr::read_csv("inst//extdata//routine//02_replace.csv")
  usethis::use_data(import_routine_replace, overwrite = TRUE)
  import_routine_replace_hfname <- readr::read_csv("inst//extdata//routine//03_replace_hfname.csv")
  usethis::use_data(import_routine_replace_hfname, overwrite = TRUE)
  import_routine_set_cluster <- readr::read_csv("inst//extdata//routine//04_cluster.csv")
  usethis::use_data(import_routine_set_cluster, overwrite = TRUE)
  meta <- readr::read_csv("inst//extdata//meta.csv")
  usethis::use_data(meta, overwrite = TRUE)
  devtools::document()
  devtools::load_all()
}
#' Devtools: development update function all combined in one command
#'
#' This command is a combination of several commands to update the package
#' It will run the following commands:
#' * `update_database()`
#' * `devtools::document()`
#' * `devtools::load_all()`
#' * `devtools::test()`
#' * `devtools::check()`
#' * `pkgdown::build_site()`
#' @return NULL
#' @export
#' @importFrom devtools document load_all test check build install
#' @importFrom pkgdown build_site
#' @seealso update_database
dev_update <- function() {
  update_database()
  devtools::document()
  devtools::load_all()
  devtools::test()
  devtools::check()
  pkgdown::build_site()
  formatR::tidy_dir(
    "R", arrow = TRUE, pipe = TRUE, brace.newline = FALSE,
    indent = 2, width.cutoff = 79, args.newline = TRUE
  )
}
#' Opinioned formatting function
#' @param p Path to format
#' @return NULL
#' @export
#' @importFrom formatR tidy_dir
sn_format <- function(p) {
  tidy_dir(
    p, comment = TRUE, blank = FALSE, indent = 2, wrap = 80,
    width.cutoff = 60, args.newline = TRUE, pipe = TRUE
  )
}
