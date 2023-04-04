#' @title Smart file reader
#'
#' @description This function is a smart file reader,
#' it will detect the file path and return the file list.
#' @param smart_path A string, the path of the file.
#' @return A list of file path.
#' @export
#' @importFrom stringr str_detect str_replace
#' @examples
#' \dontrun{
#' # get file list by year
#' smart_get_file_list_by_year(
#'   "C:/Users/username/Desktop/Bo District/Bo District *yyyy*.xlsx"
#' )
#' }
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

#' Smart file reader by year and month
#'
#' This function is a smart file reader,
#' it will detect the file path and return the file list.
#' @param smart_path A string, the path of the file.
#' @return A list of file path.
#' @export
#' @importFrom stringr str_detect str_replace
#' @examples
#' \dontrun{
#' # get file list by year and month
#' smart_get_file_list_by_year_and_month(
#'   "C:/Users/username/Desktop/Bo District/Bo District *yyyy* *mm*.xlsx"
#' )
#' }
smart_get_file_list_by_year_and_month <- function(smart_path) {
  # detect country code
  # find special character in special path parameter
  # loop through file path in the path.list
  # return valid file
  pattern <- "\\*yyyy\\* | \\*mm\\*"
  year_pattern <- "\\*yyyy\\*"
  month_pattern <- "\\*mm\\*"
  detected_pattern <- stringr::str_detect(smart_path, pattern)
  if (!(detected_pattern)) {
    stop("path do not contain pattern, please modify the year into *yyyy*")
  }
  target_years <- 1980:2030
  target_months <- 1:12
  result_file_list <- c()
  for (i in seq_along(target_years)) {
    for (j in seq_along(target_months)) {
      replacement_year <- as.character(target_years[i])
      replacement_month <- as.character(target_months[j])
      target_file_path <- file.path(stringr::str_replace(
        smart_path,
        year_pattern, replacement_year
      ))
      target_file_path <- file.path(stringr::str_replace(
        smart_path,
        month_pattern, replacement_month
      ))
    }
    if (file.exists(target_file_path)) {
      result_file_list <- append(result_file_list, target_file_path)
    }
  }
  return(result_file_list)
}

#' Read excel file by year
#'
#' The function will read all excel file follows the
#' pattern of \\*yyyy\\* in the path.
#' The function will return a nested tibble.
#' @param smart_path A string, the path of the file. The path should
#' contain the pattern of \*yyyy\*. Otherwise, the function will return
#' an error.
#' @param skip An integer, the number of rows to skip.
#' @param clean A boolean, whether to clean the data.
#'  * `TRUE`(the default) will clean the data. The cleaning process will use
#' internal database, `snt::routine_rename` to rename and replace the data.
#'  * `FALSE` will not clean the data.
#' @export
#' @importFrom readxl read_excel
#' @importFrom purrr map
#' @importFrom tibble tibble
#' @importFrom dplyr mutate
#' @rdname import
#' @seealso routine_rename
#' @examples \dontrun{
#' # read excel file by year
#' smart_read_excel_by_year(
#'   "C:/Users/username/Desktop/Bo District/Bo District *yyyy*.xlsx"
#' )
#' }
smart_read_excel_by_year <-
  function(smart_path,
           skip = 0,
           clean = TRUE) {
    file_list <-
      smart_get_file_list_by_year(smart_path)
    data_tables <-
      map(
        file_list$files,
        ~ read_excel(.x, skip = skip)
      )
    # create a nested tibble
    result <- tibble(
      year = file_list$years,
      data = data_tables
    )
    if (clean) {
      result <- result |>
        # rename using internal rename database
        mutate(data = map(
          data, routine_rename
        )) |>
        # replace using internal replace database
        mutate(data = map(
          data, routine_replace
        ))
    }
    return(result)
  }

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
        dplyr::mutate(data = purrr::map(data, ~ routine_rename(.x, cty = country))) |>
        # replace using internal replace database
        dplyr::mutate(data = purrr::map(data, ~ routine_replace(.x, cty = country)))
    }
    return(result)
  }