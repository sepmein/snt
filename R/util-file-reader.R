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
    target_file_path <- file.path(stringr::str_replace(smart_path,
                                                       pattern, replacement_year))
    if (file.exists(target_file_path)) {
      result_file_list <- append(result_file_list, target_file_path)
      result_years <- append(result_years, target_years[i])
    }
  }
  return(list(files = result_file_list,
              years = result_years))
}

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
      target_file_path <- file.path(stringr::str_replace(smart_path,
                                                         year_pattern, replacement_year))
      target_file_path <- file.path(stringr::str_replace(smart_path,
                                                         month_pattern, replacement_month))
    }
    if (file.exists(target_file_path)) {
      result_file_list <- append(result_file_list, target_file_path)
    }
  }
  return(result_file_list)
}

smart_read_excel_by_year <-
  function(reader,
           smart_path,
           skip = 0,
           clean = TRUE,
           country = snt_country) {
    if (is.null(snt_country)) {
      warning("snt::set_country method has not runned yet, this will cause problem.")
    }
    file_list <-
      reader(smart_path)
    data_tables <-
      purrr::map(file_list$files,
                 ~ readxl::read_excel(.x, skip = skip))
    # create a nested tibble
    result <- tibble::tibble(year = file_list$years,
                             data = data_tables)
    if (clean) {
      result <- result |>
        # rename using internal rename database
        dplyr::mutate(data = purrr::map(data, ~ routine_rename(.x, country = country))) |>
        # replace using internal replace database
        dplyr::mutate(data = purrr::map(data, ~ routine_replace(.x, country = country)))
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
    file_list <- purrr::keep(file_list,
                             ~ !stringr::str_starts(.x, "~"))
    # map get full path
    file_list <- file.path(smart_path, file_list)
    data_tables <-
      purrr::map(file_list,
                 ~ readxl::read_excel(.x, skip = skip))
    # create a nested tibble
    result <- tibble::tibble(file = file_list,
                             data = data_tables)
    if (clean) {
      result <- result %>%
        # rename using internal rename database
        dplyr::mutate(data = purrr::map(data, ~ routine_rename(.x, cty = country))) %>%
        # replace using internal replace database
        dplyr::mutate(data = purrr::map(data, ~ routine_replace(.x, cty = country)))
    }
    return(result)
  }
