#' @title Dissect data
#' @description Dissect data by admin level and date level
#' TODO: next step, should be able to include extra column in the data.table,
#' such as hf_type
#' @param d Data table
#' @export
#' @importFrom data.table setkey :=
dissect_meta <- function(d) {
  # test if adm1 is in d, where d is a data.table
  test_meta_columns(d)
  admin_group <- get_adm_group(d)
  date_group <- get_date_group(d)
  # gen_id_if_not_exist

  meta_admin <- gen_meta_adm(d)
  gen_id_dt(d, "id")
  # merge with meta_admin
  d <- d[meta_admin, on = admin_group]


  # remove admin_group in d, and add another column called adm_id, which is
  # the id in the meta_admin
  admin_group <- get_adm_group(d)
  d[, (admin_group) := NULL]

  # select date_group
  meta_date <- gen_meta_date(d)

  # merge with meta_date
  d <- d[meta_date, on = date_group]

  # remove date_group in d, and add another column called date_id, which is
  # the id in the meta_date
  d[, (date_group) := NULL]

  setkey(d, id, id_adm, id_date)
  # return d
  return(list(d = d, meta_admin = meta_admin, meta_date = meta_date))
}

#' @title Assemble data 🈴
#' @description Assemble data by admin level and date level
#' @param d Data table's id, id_adm, id_date
#' @param meta_adm Meta admin table's id_adm
#' @param meta_date Meta date table's id_date
#' @export
#' @importFrom data.table key merge.data.table CJ setkey
#' @examples
#' assemble_meta(d, meta_admin, meta_date)
assemble_meta <- function(meta_adm, meta_date, d = NULL) {
  # arguments should have keys d should have id, id_adm, id_date meta_adm
  # should have id_adm meta_date should have id_date test if they have the
  # above keys
  if (!all(
    key(d) ==
      c("id", "id_adm", "id_date")
  )) {
    stop("d should have id, id_adm, id_date as key")
  }
  adm_level <- get_adm_level(meta_adm)
  if (adm_level == "adm1") {
    adm_keys <- c("id_adm", "id_adm1")
  }
  if (adm_level == "adm2") {
    adm_keys <- c("id_adm", "id_adm1", "id_adm2")
  }
  if (adm_level == "hf") {
    adm_keys <- c("id_adm", "id_adm1", "id_adm2", "id_hf")
  }
  if (!all(
    key(meta_adm) ==
      adm_keys
  )) {
    stop("meta_adm should have id_adm as key")
  }

  if (!all(
    key(meta_date) ==
      c("id_date")
  )) {
    stop("meta_date should have id_date as key")
  }
  # if d is NULL, return meta_adm and meta_date
  meta <- CJ(id_adm = meta_adm$id_adm, id_date = meta_date$id_date)
  meta <- merge(meta, meta_adm, by = "id_adm")
  meta <- merge(meta, meta_date, by = "id_date")
  setkey(meta, id_adm, id_date)
  if (is.null(d)) {
    assembled <- meta
  } else {
    # full join d with meta_adm and meta_date
    assembled <- d[meta, on = c("id_adm", "id_date")]
    setkey(assembled, id, id_adm, id_date)
    adm_group <- get_adm_group
    setcolorder(
      assembled, c(
        "id", names(meta_adm),
        names(meta_date)
      )
    )
  }
  return(assembled)
}


#' @importFrom data.table setkey setcolorder
gen_meta_adm <- function(d) {
  adm_group <- get_adm_group(d)
  adm_level <- get_adm_level(d)
  # select admin_group
  meta_admin <- d[, ..adm_group]
  # get unique admins
  meta_admin <- unique(meta_admin)
  gen_id_dt(meta_admin, "id_adm")
  if (adm_level == "adm1") {
    if (!"id_adm1" %in% adm_group) {
      # get unique adm1s and gen id, then merge back to meta_admin
      adm1 <- meta_admin[, .(adm1 = unique(adm1))]
      gen_id_dt(adm1, "id_adm1")
      meta_admin <- merge(meta_admin, adm1, by = "adm1", all = TRUE)
    }
    setkey(meta_admin, id_adm, id_adm1)
    setcolorder(meta_admin, c("id_adm", "adm1", "id_adm1"))
  }

  if (adm_level == "adm2") {
    if (!"id_adm1" %in% adm_group) {
      # get unique adm1s and gen id, then merge back to meta_admin
      adm1 <- meta_admin[, .(adm1 = unique(adm1))]
      gen_id_dt(adm1, "id_adm1")
      meta_admin <- merge(meta_admin, adm1, by = "adm1", all = TRUE)
    }
    if (!"id_adm2" %in% adm_group) {
      # get unique adm2 and gen id, then merge back to meta_admin
      adm2 <- meta_admin[, .(adm2 = unique(adm2))]
      gen_id_dt(adm2, "adm2")
      meta_admin <- merge(meta_admin, adm2, by = "adm2", all = TRUE)
    }
    setkey(meta_admin, id_adm, id_adm1, id_adm2)
    setcolorder(meta_admin, c("id_adm", "adm1", "id_adm1", "adm2", "id_adm2"))
  }

  if (adm_level == "hf") {
    if (!"id_adm1" %in% adm_group) {
      # get unique adm1s and gen id, then merge back to meta_admin
      adm1 <- meta_admin[, .(adm1 = unique(adm1))]
      gen_id_dt(adm1, "id_adm1")
      meta_admin <- merge(meta_admin, adm1, by = "adm1", all = TRUE)
    }
    if (!"id_adm2" %in% adm_group) {
      # get unique adm2 and gen id, then merge back to meta_admin
      adm2 <- meta_admin[, .(adm2 = unique(adm2))]
      gen_id_dt(adm2, "adm2")
      meta_admin <- merge(meta_admin, adm2, by = "adm2", all = TRUE)
    }
    if (!"id_hf" %in% adm_group) {
      # get unique hf and gen id, then merge back to meta_admin
      hf <- meta_admin[, .(hf = unique(hf))]
      gen_id_dt(hf, "id_hf")
      meta_admin <- merge(meta_admin, hf, by = "hf", all = TRUE)
    }
    setkey(meta_admin, id_adm, id_adm1, id_adm2, id_hf)
    setcolorder(meta_admin, c("id_adm", "adm1", "id_adm1", "adm2", "id_adm2", "hf", "id_hf"))
  }
  return(meta_admin)
}

#' @importFrom data.table CJ setkey
gen_meta_date <- function(d) {
  date_group <- get_date_group(d)
  date_label <- get_date_level(d)
  dates <- d[, ..date_group]
  dates <- unique(dates)
  if (date_label == "month") {
    meta_date <- CJ(
      year = min(dates$year):max(dates$year),
      month = 1:12
    )
  } else {
    meta_date <- CJ(year = min(dates$year):max(dates$year))
  }
  gen_id_dt(meta_date, "id_date")
  setkey(meta_date, id_date)

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
    adm_group <- c("adm1", "adm2", "hf")
  } else if (admin_level == "adm2") {
    adm_group <- c("adm1", "adm2")
  } else {
    adm_group <- c("adm1")
  }
  if ("id_adm1" %in% names(d)) {
    adm_group <- c(adm_group, "id_adm1")
  }
  if ("id_adm2" %in% names(d)) {
    adm_group <- c(adm_group, "id_adm2")
  }
  if ("id_hf" %in% names(d)) {
    adm_group <- c(adm_group, "id_hf")
  }
  return(adm_group)
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
  # if hf is in d, then adm1, adm2 should be also in d Otherwise stop if adm2
  # is in d, then adm1 should be also in d Otherwise stop adm1 must be in d,
  # Otherwise stop if month is in d, then year should be also in d Otherwise
  # stop year must in d
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
