# fgeo.abundance ----------------------------------------------------------

#' Count unique trees based on the dbh of their largest stem.
#' 
#' This functions count unique treeid by censusid (automatically) and any number
#' of additional grouping variables (via `dplyr::group_by()`. Data with
#' multi-stem trees is first collapsed by picking the stem (of each treeid) with
#' maximum dbh.
#' * `count_woods()` is general and allows filtering data using any number of
#' expressions passed via the argument `...` to `dplyr::filter()`.
#' * `count_trees()` is a shortcut that defines tress as stems of dbh of 100 mm
#' and above.
#' * `count_samplings()` is a shortcut that defines saplings as stems of dbh of
#' between 10 mm inclusive and 100 mm exclusive.
#'   
#' @param .data A dataframe; particularly a ForestGEO census or ViewFullTable.
#' @param ... Expressions to pick stems of specific `dbh` -- where _stems_
#'   refers to the largest stem of each tree.
#'
#' @family functions for fgeo census and vft.
#'
#' @return A dataframe with preserved groups (if any).
#' 
#' @export
#' 
#' @examples
#' library(dplyr)
#' 
#' census <- tibble::tribble(
#'     ~sp, ~treeID, ~stemID, ~hom, ~dbh,
#'   "sp1",     "1",   "1.1",  130,   10,
#'   "sp1",     "1",   "1.2",  130,  100,
#'   "sp2",     "2",   "2.1",  130,   22,
#'   "sp2",     "2",   "2.2",  130,   99,
#'   "sp2",     "2",   "2.3",  130,   NA
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
count_woods <- fgeo.tool::pick_woods_f(count_distinct_treeid)

#' @rdname count_woods
#' @export
count_trees <- function(.data) {
  count_woods(.data, .data$dbh >= 100)
}

#' @rdname count_woods
#' @export
count_saplings <- function(.data) {
  count_woods(.data, .data$dbh >= 10, .data$dbh < 100)
}
