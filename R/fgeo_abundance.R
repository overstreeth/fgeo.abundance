#' Create tables byyr_abundance and byyr_basal_area.
#'
#' @param vft A dataframe; particularly a ForestGEO ViewFullTable.
#' @param .valid_status String giving possible values of `Status`.
#' @inheritParams fgeo.tool::filter_status
#'
#' @return A dataframe.
#' 
#' @export
#' @examples
#' vft <- data.frame(
#'   Tag = c("0001", "0001", "0002", "0002"),
#'   PlotName = "p",
#'   Status = c("alive", "dead", "alive", "missing"),
#'   DBH = c(1, 1, 10, 10),
#'   ExactDate = c("2000-01-01", "2001-01-01", "2000-01-01", "2001-01-01"),
#'   PlotCensusNumber = c(1, 2, 1, 2),
#'   CensusID = c(1, 2, 1, 2),
#'   Genus = c("A", "A", "B", "B"),
#'   SpeciesName = c("a", "a", "b", "b"),
#'   Family = "f",
#'   stringsAsFactors = FALSE
#' )
#' 
#' byyr_abundance(vft)
#' suppressWarnings(byyr_basal_area(vft))
#' basal_area(1)
#' basal_area(10)
#' 
#' # You may want to filter the data first
#' pick <- vft %>% 
#'   pick_plotname("p") %>% 
#'   fgeo.base::pick_dbh_min(10)
byyr_abundance <- function(vft, 
                           .valid_status = c(
                             "dead", "alive", "broken below", "missing")
                           ) {
  stopifnot(is.data.frame(vft))
  crucial <- c(
    "Genus", "SpeciesName", "Family", "Status", "DBH", "ExactDate", 
    "PlotCensusNumber"
  )
  fgeo.base::check_crucial_names(vft, crucial)
  
  vft %>% 
    filter_tree_status_by_census(.status = "dead", exclude = TRUE, .valid_status) %>%
    mean_years() %>% 
    drop_if_na("year") %>% 
    dplyr::count(.data$species, .data$Family, .data$year) %>% 
    tidyr::spread(.data$year, n, fill = 0) %>% 
    dplyr::arrange(.data$species, .data$Family)
}

#' @rdname byyr_abundance
#' @export
byyr_basal_area <- function(vft, 
                            .valid_status = c(
                              "dead", "alive", "broken below", "missing"
                            )) {
  vft %>% 
    filter_tree_status_by_census(
      .status = "dead", exclude = TRUE, .valid_status
    ) %>%
    mean_years() %>% 
    dplyr::group_by(.data$species, .data$Family, .data$year) %>%
    basal_area(dbh = .data$DBH) %>% 
    dplyr::arrange(.data$species, .data$Family, .data$year) %>% 
    dplyr::ungroup() %>% 
    tidyr::spread(.data$year, basal_area, fill = 0)
}

# Add `status_tree` by census and pick or drop alive or dead trees.
filter_tree_status_by_census <- function(vft, .status, exclude, .valid_status) {
  stopifnot(length(.status) == 1, .status %in% c("dead", "alive"))

  sane <- sanitize_Status_DBH_ExaxtDate(vft, .valid_status)
  
  message("Calculating tree-status (from stem `Status`) by `PlotCensusNumber`.")
  with_status_tree <- sane %>% 
    dplyr::group_by(.data$PlotCensusNumber) %>% 
    fgeo.tool::add_status_tree(status_a = "alive", status_d = "dead") %>% 
    dplyr::ungroup()
  
  filtering <- ifelse(exclude, "Dropping", "Picking")
  message(filtering, " rows where `Status = ", .status, "`.")
  fgeo.tool::filter_status(
    with_status_tree, wood = "tree", .status = .status, exclude = exclude
  )
}

mean_years <- function(vft) {
  years <-  vft %>% 
    dplyr::group_by(.data$PlotCensusNumber) %>% 
    dplyr::summarise(
      year = round(mean(lubridate::year(.data$ExactDate), na.rm = TRUE))
    ) %>% 
    unique() %>% 
    dplyr::arrange(.data$PlotCensusNumber) %>% 
    dplyr::ungroup()
  
  dplyr::left_join(vft, years, by = "PlotCensusNumber") %>% 
    dplyr::mutate(species = paste(.data$Genus, .data$SpeciesName)) %>% 
    dplyr::arrange(.data$year)
}

sanitize_Status_DBH_ExaxtDate <- function(vft, .valid_status) {
  # * status
  vft_warned <- inform_if_bad_status(vft, .valid_status)
  vft_good_status <- fix_status_if_bad_or_err(vft_warned, .valid_status)
  # * dbh
  not_na_dbh <- drop_if_missing_dbh(vft_good_status)
  # * ExactDate
  drop_if_missing_dates(not_na_dbh)
}

inform_if_bad_status <- function(vft, .valid_status) {
  status_ok <- all(sort(unique(vft$Status)) %in% .valid_status)
  if (!status_ok) {
    message(
      "Unique values of column `Status` and argument `valid_status` ", 
      "should match:\n",
      "* Status col: ", commas(sort(unique(.valid_status))), ".\n",
      "* valid_status arg: ", commas(sort(unique(vft$Status))), "."
    )
  }
  invisible(vft)
}

fix_status_if_bad_or_err <- function(vft, .valid_status) {
  status_ok <- all(sort(unique(vft$Status)) %in% .valid_status)
  if (!status_ok) {
    message("Fixing status automatically.")
    vft <- fix_bad_status(
      vft, status_col = vft$Status, status_arg = .valid_status
    )
    
    tryCatch(
      testthat::expect_silent(inform_if_bad_status(vft, .valid_status)),
      error = function(cond) {
        stop(
          "Tried but failed to fix status automatically.\n",
          "* Fix `Status` manually and retry.", 
          call. = FALSE
        )
      },
      warning = function(cond) "Failed to fix status automatically."
    )
  }
  invisible(vft)
}

fix_bad_status <- function(vft, status_col, status_arg) {
  vft$Status <- sub("^.*dead.*$", "dead", vft$Status)
  vft$Status <- sub("^.*alive.*$", "alive", vft$Status)
  vft
}

drop_if_missing_dbh <- function(dfm) {
  missing_dbh <- is.na(dfm$DBH)
  if (any(missing_dbh)) {
    warning(
      "Dropping ", sum(missing_dbh), " rows with missing `DBH` values.",
      call. = FALSE
    )
  }
  dplyr::filter(dfm, !is.na(.data$DBH))
}

drop_if_missing_dates <- function(x) {
  missing_dates <- is.na(x$ExactDate)
  if (any(missing_dates)) {
    warning("Detected and ignoring missing dates.")
  }
  x <- x[!missing_dates, , drop = FALSE]
  invisible(x)
}

# Utils -------------------------------------------------------------------

commas <- function(...) {
  paste0(..., collapse = ", ")
}
