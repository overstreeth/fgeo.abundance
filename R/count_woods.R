# fgeo.abundance ----------------------------------------------------------

#' Count main stems of a given dbh range.
#' 
#' These functions count main stems by censusid (automatically) and any number
#' of additional grouping variables specified via `dplyr::group_by()`.
#' Main stems are automatically picked via [fgeo.tool::pick_largest_hom_dbh()].
#' 
#' @description
#' `count_woods()` is a general function that takes any dataframe and any number
#' of expressions to filter that dataframe. The other functions are shortcuts:
#' * `count_trees()` picks stems of 100 mm dbh and above.
#' * `count_saplings()` picks stems between 10 mm dbh inclusive and 100 mm dbh 
#' exclusive.
#' * `count_saplings_and_trees()` picks stems of 10 mm dbh and above.
#' 
#' @param .data A dataframe; particularly a ForestGEO census or ViewFullTable.
#' @param ... Expressions to pick stems of specific `dbh` -- where _stems_
#'   refers to the largest stem of each tree.
#' 
#' @seealso [fgeo.tool::pick_largest_hom_dbh()].
#' @family functions for fgeo census and vft.
#' @aliases abundance
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
#' 
#' count_woods(by_sp, dbh >= 10)
#' # Same
#' count_saplings_and_trees(by_sp)
#' 
#' # If data contains multiple censuses, the resulting counts are by census
#' census$CensusID <- 1
#' census2 <- mutate(census, CensusID = 2, dbh = dbh + 10)
#' censuses <- bind_rows(census, census2)
#' censuses
#' 
#' count_woods(censuses, dbh >= 10)
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

#' @rdname count_woods
#' @export
count_saplings_and_trees <- function(.data) {
  count_woods(.data, .data$dbh >= 10)
}
