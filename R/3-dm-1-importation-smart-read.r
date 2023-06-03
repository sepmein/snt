#' Smart file reader
#'
#' This function is a smart file reader,
#' it will detect the file path and return the file list with dates.
#' For example, we have a file path like this:
#' 'C:/Users/username/Desktop/Bo District/Bo District 2020-09.xlsx'
#' 'C:/Users/username/Desktop/Bo District/Bo District 2020-10.xlsx'
#' 'C:/Users/username/Desktop/Bo District/Bo District 2020-11.xlsx'
#' ...
#' If we want to read all the files in the folder
#' and extract the date from the file name,
#' we can use this function to read all the files.
#' The function will detect the patterns in the file path
#' and return the file list with dates.
#' The patterns should be like this:
#' Year pattern: *yyyy*
#' Month pattern: *mm*
#' Day pattern: *dd*
#'
#' Day pattern should be used with year and month pattern. If the day pattern is found,
#' then both year and month pattern should be found. Otherwise, the function will throw an error.
#' The same rule applies to month pattern.
#' If the month pattern is found, then year pattern should be found. Otherwise, the function will throw an error.
#'
#' @param smart_path A string, the path of the file.
#' Should contain patterns defined above.
#' @return A list of file path and along with a date list.
#' The result example:
#' $files
#' [1] 'C:/Users/username/Desktop/Bo District/Bo District 2020-09.xlsx'
#' [2] 'C:/Users/username/Desktop/Bo District/Bo District 2020-10.xlsx'
#' [3] 'C:/Users/username/Desktop/Bo District/Bo District 2020-11.xlsx'
#' $dates
#' [1] '2020-09-01' '2020-10-01' '2020-11-01'
#' @export
#' @importFrom stringr str_detect str_replace
#' @importFrom lubridate make_date day month year
sn_get_files <- function(smart_path, ...) {
  # ... will be some custom patterns for example, could be
  # c('code' = '\\*ccc\\*) we need to extract args from ...
  args <- list(...)
  # detect country code find special character in special
  # path parameter loop through file path in the path.list
  # return valid file
  pattern <- "\\*yyyy\\*|\\*mm\\*|\\*dd\\*"
  year_pattern <- "\\*yyyy\\*"
  month_pattern <- "\\*mm\\*"
  day_pattern <- "\\*dd\\*"
  detected_pattern <- stringr::str_detect(smart_path, pattern)
  if (!(detected_pattern)) {
    stop(
      "Reading path do not contain pattern, please modify the year into *yyyy* and month into *mm*"
    )
  }
  is_year_pattern_found <- stringr::str_detect(smart_path, year_pattern)
  is_month_pattern_found <- stringr::str_detect(smart_path, month_pattern)
  is_day_pattern_found <- stringr::str_detect(smart_path, day_pattern)
  # if day pattern is found, then both year and month
  # pattern should be found
  if (is_day_pattern_found) {
    if (!(is_year_pattern_found && is_month_pattern_found)) {
      stop(
        "The path contains day pattern (*dd*), but does not contain year pattern (*yyyy*) and month pattern (*mm*)"
      )
    } else {
      seq_by <- "day"
    }
  } else if (is_month_pattern_found) {
    # if month pattern is found, then year pattern should
    # be found
    if (!(is_year_pattern_found)) {
      stop(
        "The path contains month pattern (*mm*), but does not contain year pattern (*yyyy*)"
      )
    } else {
      seq_by <- "month"
    }
  } else if (is_year_pattern_found) {
    seq_by <- "year"
  }
  # Generate a sequence of dates, with one date for each
  # day.
  target_date_range <- seq(
    from = lubridate::make_date(1980, 1, 1),
    to = lubridate::make_date(2050, 12, 31),
    by = seq_by
  )
  # Create empty lists for the results
  result_file_list <- c()
  result_date <- c()
  for (i in seq_along(target_date_range)) {
    if (seq_by == "day") {
      replacement_day <- as.character(day(target_date_range[i]))
      replacement_month <- as.character(month(target_date_range[i]))
      replacement_year <- as.character(year(target_date_range[i]))
      target_file_path <- file.path(
        smart_path |>
          stringr::str_replace(year_pattern, replacement_year) |>
          stringr::str_replace(
          month_pattern, stringr::str_pad(replacement_month, 2, pad = "0")
        ) |>
          stringr::str_replace(
          day_pattern, stringr::str_pad(replacement_day, 2, pad = "0")
        )
      )
      target_year <- as.numeric(replacement_year)
      target_month <- as.numeric(replacement_month)
      target_day <- as.numeric(replacement_day)
    }
    if (seq_by == "month") {
      replacement_month <- as.character(month(target_date_range[i]))
      replacement_year <- as.character(year(target_date_range[i]))
      # Get the path to the target file.
      target_file_path <- file.path(
        smart_path |>
          stringr::str_replace(year_pattern, replacement_year) |>
          stringr::str_replace(
          month_pattern, stringr::str_pad(replacement_month, 2, pad = "0")
        )
      )
      # Get the year, month, and day of the target file.
      target_year <- as.numeric(replacement_year)
      target_month <- as.numeric(replacement_month)
      target_day <- 1
    }
    if (seq_by == "year") {
      # Create a character string containing the year to be
      # replaced
      replacement_year <- as.character(year(target_date_range[i]))
      # Replace the year in the file path with the new year
      target_file_path <- file.path(
        smart_path |>
          stringr::str_replace(year_pattern, replacement_year)
      )
      # Create the year, month, and day values for the new
      # file path
      target_year <- as.numeric(replacement_year)
      target_month <- 1
      target_day <- 1
    }
    # If the new file path exists, add it to the list of
    # file paths
    if (file.exists(target_file_path)) {
      result_file_list <- append(result_file_list, target_file_path)
      # Create a date object for the new file path and add
      # it to the list of dates
      result_date <- append(
        result_date, lubridate::make_date(target_year, target_month, target_day)
      )
    }
  }
  return(list(files = result_file_list, dates = result_date))
}
#' @title Read excel files by year
#'
#' @description Read excel files by year
#' @param smart_path path to the excel files
#' @param skip number of rows to skip
#' @param clean whether to clean the data
#' @return a nested tibble
smart_read_excel_by_year <- function(smart_path, skip = 0, clean = TRUE) {
  file_list <- get_files(smart_path)
  data_tables <- map(file_list$files, ~read_excel(.x, skip = skip))
  # create a nested tibble
  result <- tibble(year = file_list$years, data = data_tables)
  if (clean) {
    result <- result |>
      # rename using internal rename database
    mutate(data = map(data, routine_rename)) |>
      # replace using internal replace database
    mutate(data = map(data, routine_replace))
  }
  return(result)
}
smart_get_all_files_in_dir <- function(smart_path, skip = 0, clean = TRUE, country = snt_country) {
  if (!(dir.exists(smart_path))) {
    stop("Path does not exists.")
  }
  file_list <- list.files(smart_path)
  # remove temp xlsx file '~$Bo District.xlsx'
  file_list <- purrr::keep(file_list, ~!stringr::str_starts(.x, "~"))
  # map get full path
  file_list <- file.path(smart_path, file_list)
  data_tables <- purrr::map(file_list, ~readxl::read_excel(.x, skip = skip))
  # create a nested tibble
  result <- tibble::tibble(file = file_list, data = data_tables)
  if (clean) {
    result <- result |>
      # rename using internal rename database
    dplyr::mutate(
      data = purrr::map(data, ~routine_rename(.x, cty = country))
    ) |>
      # replace using internal replace database
    dplyr::mutate(
      data = purrr::map(data, ~routine_replace(.x, cty = country))
    )
  }
  return(result)
}
