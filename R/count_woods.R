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
  dots <- rlang::enquos(...)
  
  flat <- collapse_treeid(.data)
  pick <- dplyr::filter(flat, !!!dots)
  abundance_tree(pick)
}

count_trees <- function(.data) {
  count_woods(.data, dbh >= 100)
}

count_saplings <- function(.data) {
  count_woods(.data, dbh >= 10, dbh < 100)
}

collapse_treeid <- function(.x) {
  # TODO: GROUP BY CENSUS ID?
  # TODO: ERR IF MULTIPLE PLOTS?
  
  stopifnot(is.data.frame(.x))
  # Lowercase names to enable using census and ViewFullTable
  .data <- rlang::set_names(.x, tolower)
  fgeo.base::check_crucial_names(.data, c("treeid", "dbh"))
  
  
  
  # FIXME: Implement do or map_df
  .data <- collapse_treeid_imp(.data)
  fgeo.base::rename_matches(.data , .x)
}

collapse_treeid_imp <- function(.data) {
  .data <- .data %>%  
    dplyr::group_by(.data$treeid) %>% 
    dplyr::arrange(desc(.data$dbh)) %>% 
    # TODO: Consider using min_rank(); see r4ds
    dplyr::filter(dplyr::row_number() == 1) %>% 
    dplyr::ungroup()
}
