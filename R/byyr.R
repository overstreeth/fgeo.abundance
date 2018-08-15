#' Create tables of abundance and basal area by year.
#' 
#' 
#' These functions work on two steps:
#' 1. For each census, they automatically pick a single stem per `treeid`, and
#' then they pick the stems within the `dbh` range you specify. This first step
#' is implemented via [fgeo.tool::pick_woods()] -- see that function for
#' details.
#' 2. For each species and each (round mean) year of measurement, these
#' functions do different things:
#'     * `abundance_byyr()` counts the number of trees.
#'     * `basal_area_byyr()` calculates the total basal area.
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
#' @param ... An expression of dbh to pick woods of a specific range.
#' @inheritParams fgeo.tool::add_status_tree
#' 
#' @seealso [fgeo.tool::pick_woods()].
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
  stopifnot(is.data.frame(vft))
  
  crucial <- c("plotname", "tag")
  low_nms  <- check_crucial_names(set_names(vft, tolower), crucial)
  prep <- prepare_byyr(low_nms, ...)
  
  out <- prep %>% 
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
  stopifnot(is.data.frame(vft))
  
  low_nms <- set_names(vft, tolower)
  prep <- prepare_byyr(low_nms, ...)
  
  out <- prep %>% 
    group_by(.data$species, .data$family, .data$year) %>%
    basal_area(dbh = .data$dbh) %>%
    arrange(.data$species, .data$family, .data$year) %>%
    ungroup() %>%
    tidyr::spread(.data$year, basal_area, fill = 0)
  
  rename_matches(out, vft)
}

prepare_byyr <- function(vft, ...) {
  dots <- lowercase_var(..., .var = "dbh")
  flag_if_not_expression_of_var(dots, .flag = rlang::abort, .var = "dbh")
  
  vft %>%
    check_prepare_byyr() %>%
    fgeo.tool::pick_woods(!!! dots) %>% 
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
