#' Create tables of abundance and basal area by year.
#' 
#' * `abundance_byyr()` first picks the main stem of each tree (see
#' ?[fgeo.tool::pick_main_stem()] and then, for each species and each
#' (round mean) year of measurement, counts the number of
#' trees. The result includes __main stems__ within a given dbh range.
#' * `basal_area_byyr()` first sums the basal basal area of all stems of each
#' tree, and then, for each species and each (round mean) year of measurement,
#' sums the basal area of all trees. The result includes all stems within a
#' given dbh range (notice the difference with `abundance_byyr()`).
#' 
#' You don't need to pick stems by status before feeding data to these
#' functions. Doing so may make your code more readable but it should not affect
#' the result. This is because the expressions passed to `...` pick data by
#' `dbh` and exclude missing the `dbh` values associated to non-alive stems,
#' including dead, missing, and gone.
#' 
#' @param vft A ForestGEO-like dataframe; particularly a ViewFullTable. As such,
#'   it should contain columns `PlotName`, `CensusID`, `TreeID`, `StemID`,
#'   `Status`, `DBH`, `Genus`, `SpeciesName`, `ExactDate`, `PlotCensusNumber`,
#'   `Family`, `Tag`, and `HOM`. `ExactDate` should contain dates from
#'   1980-01-01 to the present day in the format yyyy-mm-dd.
#' @param ... Expressions to pick main stems of a specific `dbh` range.
#' 
#' @seealso [fgeo.tool::pick_main_stem()].
#'
#' @return A dataframe.
#' 
#' @export
#' 
#' @examples 
#' library(fgeo.tool)
#' 
#' vft <- example_byyr
#' vft
#' abundance_byyr(vft, DBH >= 10, DBH < 20)
#' abundance_byyr(vft, DBH >= 10)
#' basal <- basal_area_byyr(vft, DBH >= 10)
#' # Convert units and standardize by plot size in hectares
#' years <- c("2001", "2002")
#' in_he <- convert_unit_at(basal, .at = years, from = "mm2", to = "hectare")
#' standardize_at(in_he, .at = years, denominator = 50)
abundance_byyr <- function(vft, ...) {
  low_nms  <- check_byyr(set_names(vft, tolower))
  crucial <- c("plotname", "tag")
  low_nms  <- check_crucial_names(low_nms, crucial)
  
  main_stems <- fgeo.tool::pick_main_stem(low_nms)
  
  with_years <- add_years(pick_byyr(main_stems, ...))
  out <- with_years %>% 
    group_by(.data$plotname, .data$year, .data$family, .data$species) %>%
    count_distinct_treeid() %>%
    ungroup() %>%
    select(-.data$plotname) %>%
    select(.data$species, .data$family, dplyr::everything()) %>%
    tidyr::spread(.data$year, n, fill = 0) %>%
    arrange(.data$species, .data$family)
  
  rename_matches(out, vft)
}

#' @rdname abundance_byyr
#' @export
basal_area_byyr <- function(vft, ...) {
  low_nms <- check_byyr(set_names(vft, tolower))
  
  with_years <- add_years(pick_byyr(low_nms, ...))
  out <- with_years %>% 
    group_by(.data$species, .data$family, .data$year) %>%
    basal_area(dbh = .data$dbh) %>%
    arrange(.data$species, .data$family, .data$year) %>%
    ungroup() %>%
    tidyr::spread(.data$year, basal_area, fill = 0)
  
  rename_matches(out, vft)
}

pick_byyr <- function(vft, ...) {
  dots <- lowercase_var(..., .var = "dbh")
  flag_if_not_expression_of_var(dots, .flag = rlang::abort, .var = "dbh")
  dplyr::filter(vft, !!! dots)
}

add_years <- function(x) {
  drop_if_missing_dates(x) %>% 
    mean_years() %>%
    fgeo.base::drop_if_na("year")
}

check_byyr <- function(vft) {
  stopifnot(is.data.frame(vft))
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
