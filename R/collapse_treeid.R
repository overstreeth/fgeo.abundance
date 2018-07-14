# TODO: 
# * Add argument to define how to collapse? (e.g. max min, etc.)
# * Export
# TODO: rename count_flat_trees count_flat_saplings pick_flat_treeid



#' Collapse treeid, picking only one stem (per census) per treeid.
#'
#' @param .x A dataframe; particularly a ForestGEO census or ViewFullTable.
#'
#' @return A dataframe with one row per censusid per treeid.
#' @export
#'
#' @examples
collapse_treeid <- function(.x) {
  stopifnot(is.data.frame(.x))
  .data <- rlang::set_names(.x, tolower)
  fgeo.base::check_crucial_names(.data, c("treeid", "dbh"))
  .data <- collapse_treeid_impl(.data)
  fgeo.base::rename_matches(.data , .x)
}

collapse_treeid_impl <- function(x) {
  .x <- dplyr::ungroup(x)
  
  if (multiple_plotname(.x)) {
    stop("`.x` must have a single plotname.", call. = FALSE)
  }
  
  if (multiple_censusid(.x)) {
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
