#' Count unique trees based on the dbh of their largest stem.
#' 
#' This functions count unique treeid by censusid (automatically) and any number
#' of additional grouping variables (via `dplyr::group_by()`. Data with 
#' multi-stem trees is first collapsed. The collapse defaults to picking the 
#' stem (of each treeid) with maximum dbh (pick minimum dbh with the argument
#' `.collapse`).
#' * `count_woods()` is general and allows filtering data using any number of
#'   expressions passed via the argument `...` to `dplyr::filter()`.
#' * `count_trees()` is an opinionated shortcut that defines tress as stems of
#'   dbh of 100 mm and above.
#' * `count_samplings()` is an opinionated shortcut that defines saplings as 
#'   stems of dbh of between 10 mm inclusive and 100 mm exclusive.
#'   
#' @param .data A dataframe; particularly a ForestGEO census or ViewFullTable.
#' @param ... Expressions to pick stems of specific `dbh` -- where _stems_
#'   refers to the largest stem of each tree.
#' @param .collapse `collapse_treeid_max` and `collapse_treeid_min`(bare name 
#'   of the corresponding functions) collapse the data picking the stem (of each
#'   treeid) with the maximum and minimum dbh, respectively.
#'
#' @return A dataframe with preserved groups (if any).
#' @export
#'
#' @examples
#' library(dplyr)
#' 
#' census <- tibble::tribble(
#'   ~dbh,   ~sp, ~treeID, ~stemID,
#'     10, "sp1",     "1",   "1.1",
#'    100, "sp1",     "1",   "1.2",
#'     22, "sp2",     "2",   "2.1",
#'     99, "sp2",     "2",   "2.2",
#'     NA, "sp2",     "2",   "2.3"
#' )
#' 
#' # Count all woods
#' count_woods(census)
#' 
#' by_sp_treeid <- group_by(census, sp, treeID)
#' count_trees(by_sp_treeid)
#' 
#' by_sp <- group_by(census, sp)
#' count_woods(by_sp)
#' 
#' count_woods(by_sp, dbh >= 100)
#' # Same
#' count_trees(by_sp)
#' 
#' count_woods(by_sp, dbh > 10, dbh < 100)
#' # Same
#' count_saplings(by_sp)
count_woods <- function(.data, ..., .collapse = collapse_treeid_max) {
  stopifnot(is.data.frame(.data))
  # Lowercase names and groups for work with both census and ViewFullTable
  .x <- set_names(.data, tolower)
  .x <- dplyr::grouped_df(.x, tolower(dplyr::group_vars(.x)))
  
  if (multiple_plotname(.x)) {
    stop("`.x` must have a single plotname.", call. = FALSE)
  }
  
  if (multiple_censusid(.x)) {
    .x <- fgeo.base::drop_if_na(.x, "censusid")
    .x <- dplyr::group_by(.x, .data$censusid, add = TRUE)
  }
  
  # do() prefferred to by_group() to not drop empty groups (they result in 0L)
  dots <- rlang::enquos(...)
  out <- dplyr::do(.x, count_woods_impl(., !!! dots, .collapse = .collapse))
  
  # Restore original names; then original groups
  out <- rename_matches(out, .data)
  dplyr::grouped_df(dplyr::ungroup(out), dplyr::group_vars(.data))
}

count_woods_impl <- function(.data, ..., .collapse) {
  .dots <- rlang::enquos(...)
  pick <- dplyr::filter( .collapse(.data), !!! .dots)
  count_distinct_treeid(pick)
}

#' @rdname count_woods
#' @export
count_trees <- function(.data, .collapse = collapse_treeid_max) {
  count_woods(.data, .data$dbh >= 100, .collapse = .collapse)
}

#' @rdname count_woods
#' @export
count_saplings <- function(.data, .collapse = collapse_treeid_max) {
  count_woods(.data, .data$dbh >= 10, .data$dbh < 100,.collapse = .collapse)
}
