#' Create tables of abundance and basal area by (round mean) year.
#'
#' @param vft A dataframe; particularly a ForestGEO ViewFullTable.
#'
#' @return A dataframe.
#' 
#' @examples
#' # Or load all fgeo packages in one step with library(fgeo)
#' library(fgeo.tool)
#' library(fgeo.base)
#'   
#' vft <- tibble(
#'   PlotName = c("luq", "luq", "luq", "luq", "luq", "luq", "luq", "luq"),
#'   CensusID = c(1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L),
#'   TreeID = c(1L, 1L, 2L, 2L, 1L, 1L, 2L, 2L),
#'   StemID = c(1.1, 1.2, 2.1, 2.2, 1.1, 1.2, 2.1, 2.2),
#'   Status = c("alive", "dead", "alive", "alive", "alive", "gone",
#'     "dead", "dead"),
#'   DBH = c(1L, NA, 2L, 3L, 2L, NA, 3L, 4L),
#'   Genus = c("Gn", "Gn", "Gn", "Gn", "Gn", "Gn", "Gn", "Gn"),
#'   SpeciesName = c("spp", "spp", "spp", "spp", "spp", "spp", "spp", "spp"),
#'   ExactDate = c("2001-01-01", "2001-01-01", "2001-01-01", "2001-01-01",
#'     "2002-01-01", "2002-01-01", "2002-01-01",
#'     "2002-01-01"),
#'   PlotCensusNumber = c(1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L),
#'   Family = c("f", "f", "f", "f", "f", "f", "f", "f"),
#'   Tag = c(1L, 1L, 2L, 2L, 1L, 1L, 2L, 2L)
#' )
#' vft
#' 
#' # All trees are of the same species. There are two trees, each with two stems.
#' # In census 1, the count of alive trees should be 2 because both trees are alive,
#' #   but note that one stem is dead (StemID = 1.2).
#' # In census 2 the count of alive trees should be 1:
#' #   * One tree is alive (TreeID = 1) although one stem is gone (StemID = 1.2);
#' #   * One tree is dead (TreeID = 2) because both its stems are dead.
#' 
#' vft <- vft %>% 
#'   # Determine tree status based on stem status.
#'   fgeo.tool::add_status_tree(status_a = "alive", status_d = "dead") %>%
#'   # Remove dead trees.
#'   fgeo.tool::drop_dead_tree(.status = "dead")
#' 
#' byyr_abundance(vft)
#' 
#' ba <- byyr_basal_area(vft)
#' 
#' # Convert units and standardize by plot size in hectares
#' years <- c("2001", "2002")
#' in_he <- convert_unit_at(ba, .at = years, from = "mm2", to = "hectare")
#' standardize_at(in_he, .at = years, denominator = 50)
#' @name byyr
NULL

#' @rdname byyr
#' @export
byyr_abundance <- function(vft) {
  # TODO: WARN IF DEAD TREES
  crucial <- c("PlotName", "Tag")
  vft %>% 
    check_byyr() %>% 
    check_crucial_names(crucial) %>% 
    drop_if_missing_dates() %>% 
    mean_years() %>% 
    fgeo.base::drop_if_na("year") %>% 
    dplyr::ungroup() %>% 
    dplyr::group_by(.data$PlotName, .data$year, .data$Family, .data$species) %>% 
    abundance_tree() %>% 
    dplyr::ungroup() %>% 
    dplyr::select(-.data$PlotName) %>% 
    dplyr::select(.data$species, .data$Family, dplyr::everything()) %>% 
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
  check_crucial_names(vft, crucial)
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
