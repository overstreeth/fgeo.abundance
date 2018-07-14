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

# TODO: 
# * Add argument to define how to collapse? (e.g. max min, etc.)
# * Export
collapse_treeid <- function(.x) {
  stopifnot(is.data.frame(.x))
  .data <- rlang::set_names(.x, tolower)
  fgeo.base::check_crucial_names(.data, c("treeid", "dbh"))
  .data <- collapse_treeid_imp(.data)
  fgeo.base::rename_matches(.data , .x)
}

collapse_treeid_imp <- function(x) {
  # TODO: ERR IF MULTIPLE PLOTS?
  # TODO: rename count_flat_trees count_flat_saplings pick_flat_treeid
  
  .x <- dplyr::ungroup(x)
  
  if ("censusid" %in% names(.x)) {
    # FIXME ADD THIS LINE
    .x <- fgeo.base::drop_if_na(.x, "censusid")
    .x <- dplyr::group_by(.x, .data$censusid)
  }
    
  .x %>%
    dplyr::group_by(.data$treeid, add = TRUE) %>% 
    dplyr::arrange(desc(.data$dbh)) %>% 
    # TODO: Consider using min_rank(); see r4ds
    dplyr::filter(dplyr::row_number() == 1) %>% 
    dplyr::ungroup() %>% 
    dplyr::grouped_df(dplyr::group_vars(x))
}

