#' Create tables of abundance and basal area by (round mean) year.
#'
#' @param vft A ForestGEO-like dataframe; particularly a ViewFullTable.
#' @inheritParams fgeo.tool::add_status_tree
#'
#' @return A dataframe.
#' 
#' @export
#' 
#' @examples 
#' library(fgeo.tool)
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
#'   Tag = c(1L, 1L, 2L, 2L, 1L, 1L, 2L, 2L),
#'   HOM = rep(130, 8)
#' )
#' vft
#' 
#' # All trees are of the same species. There are two trees, each with two stems.
#' # In census 1, the count of alive trees should be 2 because both trees are
#' #   alive, but note that one stem is dead (StemID = 1.2).
#' # In census 2 the count of alive trees should be 1:
#' #   * One tree is alive (TreeID = 1) although one stem is gone (StemID = 1.2);
#' #   * One tree is dead (TreeID = 2) because both its stems are dead.
#' largest_stems <- pick_largest_hom_dbh(vft)
#' abundance_byyr(largest_stems)
#' 
#' ba <- basal_area_byyr(largest_stems)
#' 
#' # Convert units and standardize by plot size in hectares
#' years <- c("2001", "2002")
#' in_he <- convert_unit_at(ba, .at = years, from = "mm2", to = "hectare")
#' standardize_at(in_he, .at = years, denominator = 50)
abundance_byyr <- function(vft, status_a = "alive", status_d = "dead") {
  crucial <- c("plotname", "tag")
  vft %>%
    set_names(tolower) %>%
    check_crucial_names(crucial) %>%
    prepare_byyr(status_a, status_d) %>%
    group_by(.data$plotname, .data$year, .data$family, .data$species) %>%
    count_distinct_treeid() %>%
    ungroup() %>%
    select(-.data$plotname) %>%
    select(.data$species, .data$family, dplyr::everything()) %>%
    tidyr::spread(.data$year, n, fill = 0) %>%
    arrange(.data$species, .data$family) %>%
    rename_matches(vft)
}

#' @rdname abundance_byyr
#' @export
basal_area_byyr <- function(vft, status_a = "alive", status_d = "dead") {
  vft %>%
    set_names(tolower) %>%
    prepare_byyr(status_a, status_d) %>%
    group_by(.data$species, .data$family, .data$year) %>%
    basal_area(dbh = .data$dbh) %>%
    arrange(.data$species, .data$family, .data$year) %>%
    ungroup() %>%
    tidyr::spread(.data$year, basal_area, fill = 0) %>%
    rename_matches(vft)
}

prepare_byyr <- function(vft, status_a, status_d) {
  vft %>%
    check_prepare_byyr() %>%
    fgeo.tool::add_status_tree(status_a, status_d) %>%
    fgeo.tool::drop_dead_tree(status_d) %>%
    drop_if_missing_dates() %>%
    mean_years() %>%
    fgeo.base::drop_if_na("year") %>%
    ungroup()
}

check_prepare_byyr <- function(vft) {
  crucial <- c(
    "genus", "speciesname", "family", "status", "dbh", "exactdate",
    "plotcensusnumber"
  )
  check_crucial_names(vft, crucial)
  
  dates <- unique(vft$exactdate)
  if (all(is.na(lubridate::ymd(dates)))) {
    abort(
      "Can't parse `exactdates`. Try parsing dates with `lubridate::ymd()`."
    )
  }
  
  too_early <- lubridate::ymd(dates) < lubridate::ymd("1980-01-01")
  too_late <- lubridate::ymd(dates) > lubridate::today()
  if (any(too_early || too_late)) {
    warn("Dates should be from 1980-present and have format yyy-mm-dd.")
  }
  
  invisible(vft)
}

mean_years <- function(vft) {
  years <- vft %>%
    set_names(tolower) %>%
    group_by(.data$plotcensusnumber) %>%
    summarize(
      year = round(mean(lubridate::year(.data$exactdate), na.rm = TRUE))
    ) %>%
    unique() %>%
    arrange(.data$plotcensusnumber) %>%
    ungroup() %>%
    rename_matches(vft)

  dplyr::left_join(vft, years, by = "plotcensusnumber") %>%
    mutate(species = paste(.data$genus, .data$speciesname)) %>%
    arrange(.data$year)
}

drop_if_missing_dates <- function(x) {
  missing_dates <- is.na(insensitive(x)$exactdate)
  if (any(missing_dates)) {
    warning("Detected and ignoring missing dates.")
  }
  x <- x[!missing_dates, , drop = FALSE]
  invisible(x)
}
