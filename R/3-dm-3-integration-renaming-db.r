#' Database for Rename column names in Routine Data
#'
#' A dataset containing the country code
#' original column names and the formatted column names
#' import_routine_rename.
#' @format A tibble with BDI:
#' \describe{
#'   \item{country}{country code, ISO3 code}
#'   \item{from}{column name to be formatted, string}
#'   \item{to}{column name to be formatted, string}
#'   ...
#' }
"import_routine_rename"
#' Database for Replace column values in Routine Data
#'
#' A dataset containing the country code
#' original value, and the formatted value
#' import_routine_replace.
#'
#' @format A tibble with BDI:
#' \describe{
#'   \item{country}{country code, ISO3 code}
#'   \item{from}{value to be formatted, string}
#'   \item{to}{value to be formatted, string}
#'   ...
#' }
"import_routine_replace"
#' Database for Replace health facilities in Routine Data
#'
#' A dataset containing the country code, district name
#' original health facilities name, and the formatted health facilities name
#' import_routine_replace_hfname.
#'
#' @format A tibble with BDI:
#' \describe{
#'   \item{country}{country code, ISO3 code}
#'   \item{district}{district name, string}
#'   \item{from}{hf name to be formatted, string}
#'   \item{to}{hf name to be formatted, string}
#'   ...
#' }
"import_routine_replace_hfname"
#' Database for Set Adm2 Cluster in Routine Data
#'
#' A dataset containing the country code, adm2, and the cluster
#' import_routine_set_cluster.
#'
#' @format A tibble with BDI:
#' \describe{
#'   \item{country}{country code, ISO3 code}
#'   \item{adm2}{adm2 name, string}
#'   \item{cluster}{cluster, int}
#'   ...
#' }
"import_routine_set_cluster"
