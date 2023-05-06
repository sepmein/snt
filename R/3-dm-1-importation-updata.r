#' @title Dissect data
#' @description Dissect data by admin level and date level
#' @param d Data table
#' @export
#' @import data.table
dissect_meta <- function(d) {
  # test if adm1 is in d, where d is a data.table
  test_meta_columns(d)
  admin_group <- get_adm_group(d)
  date_group <- get_date_group(d)
  # gen_id_if_not_exist
  gen_id_dt(d, "id")

  meta_admin <- gen_meta_adm(d)
  # merge with meta_admin
  d <- d[meta_admin, on = admin_group]

  # remove admin_group in d, and add another column called adm_id,
  # which is the id in the meta_admin
  d[, (admin_group) := NULL]

  # select date_group
  meta_date <- gen_meta_date(d)

  # merge with meta_date
  d <- d[meta_date, on = date_group]

  # remove date_group in d, and add another column called date_id,
  # which is the id in the meta_date
  d[, (date_group) := NULL]

  setkey(d, id, id_adm, id_date)
  setkey(meta_admin, id_adm)
  setkey(meta_date, id_date)
  # return d
  return(list(
    d = d,
    meta_admin = meta_admin,
    meta_date = meta_date
  ))
}

#' @title Assemble data 🈴
#' @description Assemble data by admin level and date level
#' @param d Data table's id, id_adm, id_date
#' @param meta_adm Meta admin table's id_adm
#' @param meta_date Meta date table's id_date
#' @export
#' @import data.table key merge.data.table
#' @examples
#' assemble_meta(d, meta_admin, meta_date)
assemble_meta <- function(d, meta_adm, meta_date) {
  # arguments should have keys
  # d should have id, id_adm, id_date
  # meta_adm should have id_adm
  # meta_date should have id_date
  # test if they have the above keys

  if (!all(key(d) == c("id", "id_adm", "id_date"))) { 
    stop("d should have id, id_adm, id_date as key")
  }
  if (!all(key(meta_adm) == c("id_adm"))) {
    stop("meta_adm should have id_adm as key")
  }

  if (!all(key(meta_date) == c("id_date"))) {
    stop("meta_date should have id_date as key")
  }
  assembled <- merge.data.table(d, meta_adm)
  asembled <- merge.data.table(assembled, meta_date)
  return(assembled)
}


gen_meta_adm <- function(d) {
  admin_group <- get_adm_group(d)
  # select admin_group
  meta_admin <- d[, ..admin_group]
  # get unique admins
  meta_admin <- unique(meta_admin)
  gen_id_dt(meta_admin, "id_adm")

  return(meta_admin)
}

gen_meta_date <- function(d) {
  date_group <- get_date_group(d)
  date_label <- get_date_level(d)
  dates <- d[, ..date_group]
  dates <- unique(dates)
  if (date_label == "month") {
    meta_date = CJ(
      year = min(dates$year):max(dates$year),
      month = 1:12
    )
  } else {
    meta_date = CJ(year = min(dates$year):max(dates$year))
  }
  gen_id_dt(meta_date, "id_date")

  return(meta_date)
}

get_adm_level <- function(d) {
  if ("hf" %in% colnames(d)) {
    return("hf")
  } else if ("adm2" %in% colnames(d)) {
    return("adm2")
  } else if ("adm1" %in% colnames(d)) {
    return("adm1")
  } else {
    stop("adm1, adm2, hf must be in d")
  }
}

get_adm_group <- function(d) {
  admin_level <- get_adm_level(d)
  if (admin_level == "hf") {
    return(c("adm1", "adm2", "hf"))
  } else if (admin_level == "adm2") {
    return(c("adm1", "adm2"))
  } else {
    return(c("adm1"))
  }
}

get_date_level <- function(d) {
  if ("month" %in% colnames(d)) {
    return("month")
  } else if ("year" %in% colnames(d)) {
    return("year")
  } else {
    stop("month, year must be in d")
  }
}

get_date_group <- function(d) {
  date_level <- get_date_level(d)
  if (date_level == "month") {
    return(c("year", "month"))
  } else {
    return(c("year"))
  }
}

test_meta_columns <- function(d) {
  column_name <- colnames(d)
  # if hf is in d, then adm1, adm2 should be also in d
  # Otherwise stop
  # if adm2 is in d, then adm1 should be also in d
  # Otherwise stop
  # adm1 must be in d, Otherwise stop
  # if month is in d, then year should be also in d
  # Otherwise stop
  # year must in d
  if (!("year" %in% column_name)) {
    stop("year must be in d")
  }
  if (!("adm1" %in% column_name)) {
    stop("adm1 must be in d")
  }
  if ("hf" %in% column_name) {
    if (!("adm2" %in% column_name)) {
      stop("adm2 must be in d")
    }
  } else if ("adm2" %in% column_name) {
    if (!("adm1" %in% column_name)) {
      stop("adm1 must be in d")
    }
  }
  if ("month" %in% column_name) {
    if (!("year" %in% column_name)) {
      stop("year must be in d")
    }
  }
}
