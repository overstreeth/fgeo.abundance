#' Create tables of abundance and basal area by (round mean) year.
#'
#' @param vft A dataframe; particularly a ForestGEO ViewFullTable.
#'
#' @return A dataframe.
#' 
#' @examples
#' library(fgeo.tool)
#' library(fgeo.base)
#' 
#' vft <- data.frame(
#'   Tag = c("0001", "0001", "0002", "0002"),
#'   PlotName = "p",
#'   Status = c("dead", "dead", "alive", "alive"),
#'   DBH = c(NA, NA, 10, 100),
#'   ExactDate = c("2000-01-01", "2001-01-01", "2000-01-01", "2001-01-01"),
#'   PlotCensusNumber = c(1, 2, 1, 2),
#'   CensusID = c(1, 2, 1, 2),
#'   Genus = c("A", "A", "B", "B"),
#'   SpeciesName = c("a", "a", "b", "b"),
#'   Family = "f",
#'   stringsAsFactors = FALSE
#' )
#' vft
#' 
#' # First pick the data you want
#' pick1 <- fgeo.base::pick_plotname(vft, "p")
#' pick2 <- drop_dead_trees_by_cns(pick1)
#' pick3 <- pick_dbh_min(pick2, 10)
#' pick3
#' 
#' byyr_abundance(pick3)
#' 
#' ba <- byyr_basal_area(pick3)
#' ba
#' 
#' # Convert units and standardize by plot size in hectares
#' years <- c("2000", "2001")
#' in_he <- conv_unit_at(ba, .at = years, from = "mm2", to = "hectare")
#' standardize_at(in_he, .at = years, total = 50)
#' @name byyr
NULL

#' @rdname byyr
#' @export
byyr_abundance <- function(vft) {
  check_byyr(vft) %>% 
    drop_if_missing_dates() %>% 
    mean_years() %>% 
    fgeo.base::drop_if_na("year") %>% 
    dplyr::count(.data$species, .data$Family, .data$year) %>% 
    tidyr::spread(.data$year, n, fill = 0) %>% 
    dplyr::arrange(.data$species, .data$Family)
}

#' @rdname byyr
#' @export
byyr_basal_area <- function(vft) {
  check_byyr(vft) %>% 
    drop_if_missing_dates() %>% 
    mean_years() %>% 
    dplyr::group_by(.data$species, .data$Family, .data$year) %>%
    basal_area(dbh = .data$DBH) %>% 
    dplyr::arrange(.data$species, .data$Family, .data$year) %>% 
    dplyr::ungroup() %>% 
    tidyr::spread(.data$year, basal_area, fill = 0)
}

check_byyr <- function(vft) {
  stopifnot(is.data.frame(vft))
  crucial <- c(
    "Genus", "SpeciesName", "Family", "Status", "DBH", "ExactDate", 
    "PlotCensusNumber"
  )
  fgeo.base::check_crucial_names(vft, crucial)
  invisible(vft)
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

sanitize_Status <- function(vft, .valid_status) {
  vft_warned <- inform_if_bad_status(vft, .valid_status)
  fix_status_if_bad_or_err(vft_warned, .valid_status)
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

drop_if_missing_dates <- function(x) {
  missing_dates <- is.na(x$ExactDate)
  if (any(missing_dates)) {
    warning("Detected and ignoring missing dates.")
  }
  x <- x[!missing_dates, , drop = FALSE]
  invisible(x)
}
