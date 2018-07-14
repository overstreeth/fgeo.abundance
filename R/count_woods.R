#' Count unique trees based on the dbh of their largest stem.
#' 
#' This functions help you to count unique treeid in each census:
#' * `count_woods()` is general and allows you to filter the data using any 
#'   expressions passed via the argument `...` to `dplyr::filter()`.
#' * `count_trees()` is an opinionated shortcut that defines tress as stems of
#'   dbh of 100 mm and above.
#' * `count_samplings()` is an opinionated shortcut that defines saplings as 
#'   stems of dbh of between 10 mm inlcusive and 100 mm exclusive.
#'   
#' @param .data A dataframe; particularly a ForestGEO census or ViewFullTable.
#' @param ... Expressions to pick stems of specific `dbh` -- where _stems_
#'   refers to the largest stem of each tree.
#'
#' @return
#' @export
#'
#' @examples
count_woods <- function(.data, ..., .collapse = collapse_treeid_max) {
  stopifnot(is.data.frame(.data))
  
  .x <- set_names(.data, tolower)
  # FIXME repeat this approach in collapse_treeid
  .x <- dplyr::grouped_df(.x, tolower(dplyr::group_vars(.x)))
  
    
  if (multiple_plotname(.x)) {
    stop("`.x` must have a single plotname.", call. = FALSE)
  }
  
  if (multiple_censusid(.x)) {
    .x <- fgeo.base::drop_if_na(.x, "censusid")
    .x <- dplyr::group_by(.x, .data$censusid, add = TRUE)
  }
  
  dots <- rlang::enquos(...)
  
  # # FIXME REMOVE
  # out <- by_group(
  #   .x,
  #   count_woods_impl, !!!dots, .collapse = .collapse
  # )
  
  # # FIXME repeat this approach in collapse_treeid
  out <- dplyr::do(.x, count_woods_impl(., !!! dots, .collapse = .collapse))
  # Restore original names
  out <- rename_matches(out, .data)
  # Restore original groups
  dplyr::grouped_df(dplyr::ungroup(out), dplyr::group_vars(.data))
}

count_woods_impl <- function(.data, ..., .collapse) {
  .dots <- rlang::enquos(...)
  pick <- dplyr::filter( .collapse(.data), !!! .dots)
  abundance_tree(pick)
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
