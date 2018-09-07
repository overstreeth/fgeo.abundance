#' Abundance and basal area.
#' 
#' * `abundance()` and `basal_area()`:
#'     * Warn if they detect multiple censusid and multiple plots.
#'     * Can compute by groups of data created with [group_by()].
#' * `abundance()`:
#'     * Is similar to [dplyr::n()].
#'     * Warns if it detects duplicated values of treeid.
#' * `basal_area()`:
#'     * Warns if it detects duplicated values of stemid.
#'     * Does not convert units. For that see [convert_unit()].
#' 
#' @param x A dataframe. `basal_area()` requires a column named `dbh` (case
#'   insensitive).
#' @seealso [dplyr::n()], [group_by()], [convert_unit()].
#' 
#' @section Warning:
#' To pick specific rows (e.g. to pick "alive" stems or `dbh` within some range)
#' you should use functions such as `[`, [subset()], or [dplyr::filter()]
#' _before_ using `abundance()`. (Notice the difference with `ctfs::abundance`,
#' which includes arguments to deal with `dbh` and `status`).
#' 
#' @examples
#' # abundance() -------------------------------------------------------------
#' 
#' # Similar to dplyr::n()
#' abundance(data.frame(1))
#' 
#' vft <- tibble::tribble(
#'   ~PlotName, ~CensusID, ~TreeID, ~StemID, ~DBH,
#'   "p",         1,     "1",   "1.1",   10,
#'   "q",         2,     "1",   "1.1",   10
#' )
#' 
#' # * Warns if it detects multiple values of censusid or plotname
#' # * Also warns if it detects duplicated values of treeid
#' abundance(vft)
#' 
#' # You should probably work with a single plotname. 
#' # Yet your data may have multiple stems per treeid and even multiple measures
#' # per stemid (when trees have buttressess).
#' vft2 <- tibble::tribble(
#'   ~CensusID, ~TreeID, ~StemID, ~DBH, ~HOM,
#'   1,     "1",   "1.1",   88,  130,
#'   1,     "1",   "1.1",   10,  160,
#'   1,     "2",   "2.1",   20,  130,
#'   1,     "2",   "2.2",   30,  130,
#' )
#' 
#' # You should count only the main stem of each tree
#' (main_stem <- pick_main_stem(vft2))
#' abundance(main_stem)
#' 
#' vft3 <- tibble::tribble(
#'   ~CensusID, ~TreeID, ~StemID, ~DBH, ~HOM,
#'   1,     "1",   "1.1",   20,  130,
#'   1,     "1",   "1.2",   10,  160,  # Main stem
#'   2,     "1",   "1.1",   12,  130,
#'   2,     "1",   "1.2",   22,  130   # Main stem
#' )
#' 
#' # You can compute by groups
#' (main_stems_by_census <- pick_main_stem(by_census))
#' abundance(main_stems_by_census)
#' 
#' # basal_area() ------------------------------------------------------------
#' 
#' # Data must have a column named dbh (case insensitive)
#' basal_area(data.frame(dbh = 1))
#' 
#' # * Warns if it detects multiple values of censusid or plotname
#' # * Also warns if it detects duplicated values of stemid
#' basal_area(vft)
#' 
#' # First you may pick the main stemid of each stem
#' (main_stemids <- pick_main_stemid(vft2))
#' basal_area(main_stemids)
#' 
#' # You can compute by groups
#' by_census <- group_by(vft3, CensusID)
#' basal_area(by_census)
#' 
#' # To convert units see ?convert_unit()
#' ba <- basal_area(by_census)
#' convert_unit_at(ba, .at = "basal_area", from = "mm2", to = "hectare")
#' @name abundance
NULL

with_anycase_group_df <- function(.summary, side_effects) {
  function(x) {
    low_nms <- groups_lower(set_names(x, tolower))
    lapply(side_effects, function(.f) .f(low_nms))
    result <- .summary(low_nms)
    restore_input_names_output_groups(result, x)
  }
}

#' @export
#' @rdname abundance
abundance <- with_anycase_group_df(
  abundance_df, list(warn_if_needed_treeid, warn_if_needed_plotname_censusid)
)

#' @export
#' @rdname abundance
basal_area <- with_anycase_group_df(
  basal_area_df, list(warn_if_needed_stemid, warn_if_needed_plotname_censusid)
)



abundance_df <- function(x) {
  g <- dplyr::group_vars(x)
  out <- summarize(x, n = n())
  dplyr::grouped_df(out, g)
}

basal_area_df <- function(x) {
  g <- dplyr::group_vars(x)
  out <- summarize(x, basal_area = sum(basal_area_dbl(dbh), na.rm = TRUE))
  dplyr::grouped_df(out, g)
}

basal_area_dbl <- function(x) {
  stopifnot(length(x) > 0)
  1 / 4 * pi * (x)^2
}



# Only if data contains specific `name`s.
warn_if_needed_plotname_censusid <- function(.x) {
  warn_if_has_var(
    .x, name = "censusid", predicate = is_multiple,
    problem = "Multiple", hint = "Do you need to group by censusid?"
  )
  warn_if_has_var(
    .x, name = "plotname", predicate = is_multiple,
    problem = "Multiple", hint = "Do you need to pick a single plot?"
  )
  
  invisible(.x)
}

warn_if_needed_treeid <- function(.x) {
  warn_if_has_var(
    .x, name = "treeid", predicate = is_duplicated,
    problem = "Duplicated", hint = "Do you need to pick main stems?"
  )
  invisible(.x)
}

warn_if_needed_stemid <- function(.x) {
  warn_if_has_var(
    .x, name = "stemid", predicate = is_duplicated,
    problem = "Duplicated", hint = "Do you need to pick largest `hom` values?"
  )
  invisible(.x)
}

warn_if_has_var <- function(.x, name, predicate, problem, hint) {
  if (hasName(.x, name)) {
    msg <- glue("`{name}`: {problem} values were detected. {hint}")
    flag_if_group(.x, name, predicate, warn, msg)
  }
}
