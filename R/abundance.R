#' Count distinct occurrences of a variable by groups (tree or stem abundance).
#' 
#' These functions are usually used on grouped data created by
#' `dplyr::group_by()` (reexported by __fgeo__). The output will have one row
#' for each group. 
#' * `count_distinct()` counts distinct occurrences of a variable.
#' * `abundance_stem()` and `abundance_tree()` are simpler wrappers for the
#' specifically counting distinct occurrences of the variables `stemID` and
#' `treeID` which uniquely identify each stem and tree in census datasets of
#' ForestGEO (both stem and tree tables).
#' 
#' @param .data A dataframe, commonly grouped with `group_by()`.
#' @param .var A variable to count distinct occurrences.
#' 
#' @seealso [dplyr::group_by()], [dplyr::summarise()].
#' 
#' @return An object of the same class as .data. One grouping level will be
#'   dropped.
#' @export
#'
#' @examples
#' library(dplyr)
#' 
#' # Example data
#' census <- tibble::tibble(
#'   treeID = c(1, 1, 2, 3, 3, 3),
#'   stemID = c(1, 2, 3, 4, 5, 6),
#'   quadrat = paste0("000", rep(1:2, each = 3)),
#'   sp = c(paste0("sp", c(1, 1, 2)), paste0("sp", c(3, 3, 3))),
#'   dbh = abs(sample(rnorm(100), 6) * 10)
#' )
#' census
#' 
#' count_distinct(census, stemID)
#' 
#' by_quad <- group_by(census, quadrat)
#' count_distinct(by_quad, stemID)
#' count_distinct(by_quad, treeID)
#' 
#' abundance_stem(by_quad)
#' abundance_tree(by_quad)
#' 
#' by_sp <- group_by(census, sp)
#' abundance_stem(by_sp)
#' abundance_tree(by_sp)
#' 
#' by_quad_sp <- group_by(census, quadrat, sp)
#' abundance_stem(by_quad_sp)
#' abundance_tree(by_quad_sp)
count_distinct <- function(.data, .var) {
  .var <- enquo(.var)
  dplyr::summarise(.data, n = dplyr::n_distinct(!! .var))
}

#' @export
#' @rdname count_distinct
abundance_stem <- function(.data) {
  check_crucial_names(insensitive(.data), "stemid")
  out <- count_distinct(insensitive(.data), .data$stemid)
  nms_restore_matching(out, .data)
}

#' @export
#' @rdname count_distinct
abundance_tree <- function(.data) {
  check_crucial_names(insensitive(.data), "treeid")
  out <- count_distinct(insensitive(.data), .data$treeid)
  nms_restore_matching(out, .data)
}
