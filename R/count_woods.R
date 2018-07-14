#' Count unique trees based on the dbh of their largest stem.
#'
#' @param .data A dataframe; particularly a ForestGEO census or ViewFullTable.
#' @param ... Expressions to pick stems of specific `dbh` -- where _stems_
#'   refers to the largest stem of each tree.
#'
#' @return
#' @export
#'
#' @examples
count_woods <- function(.data, ...) {
  stopifnot(is.data.frame(.data))
  .x <- set_names(.data, tolower)
  
  dots <- rlang::enquos(...)
  out <- by_group(.x, count_woods_impl, !!!dots)
  rename_matches(out, .data)
}

count_woods_impl <- function(.x, ...) {
  .dots <- rlang::enquos(...)
  flat <- collapse_treeid(.x)
  pick <- dplyr::filter(flat, !!! .dots)
  abundance_tree(pick)
}

#' @rdname count_woods
#' @export
count_trees <- function(.data) {
  count_woods(.data, dbh >= 100)
}

#' @rdname count_woods
#' @export
count_saplings <- function(.data) {
  count_woods(.data, dbh >= 10, dbh < 100)
}
