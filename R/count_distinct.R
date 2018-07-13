#' Abundance of trees, stems, and count distinct values of a variable by groups.
#'
#' These functions count rows by groups (if you need to exclude some
#' observations, to it first -- see warning). These functions are usually used
#' on grouped data created by `dplyr::group_by()` (reexported by __fgeo__). The
#' output will have one row for each group:
#' * `count_distinct()` counts distinct occurrences of a variable.
#' * `count_distinct_stemid()` and `count_distinct_treeid()` are simpler
#' wrappers for the specifically counting distinct occurrences of the variables
#' `stemID` and `treeID`, which uniquely identify each stem and tree in census
#' datasets of ForestGEO (both stem and tree tables) -- these functions are 
#' synonyms of `abundance_stem()` and `abundance_tree()`.
#'
#' @section Warning:
#' These functions do not remove dead stems or trees. If you don't want dead
#' trees to be included, remove them first (see [drop_dead_tree()],
#' [drop_dead_stem()].
#'
#' @param .data A dataframe, commonly grouped with `group_by()`.
#' @param .var A variable to count distinct occurrences.
#' 
#' @seealso [dplyr::group_by()], [dplyr::summarise()], [drop_dead_tree()],
#' [drop_dead_stem()].
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
#' count_distinct_stemid(by_quad)
#' # Same
#' abundance_stem(by_quad)
#' 
#' count_distinct_treeid(by_quad)
#' # Same
#' abundance_tree(by_quad)
#'
#' by_sp <- group_by(census, sp)
#' count_distinct_stemid(by_sp)
#' count_distinct_treeid(by_sp)
#'
#' by_quad_sp <- group_by(census, quadrat, sp)
#' count_distinct_stemid(by_quad_sp)
#' count_distinct_treeid(by_quad_sp)
count_distinct <- function(.data, .var) {
  .var <- enquo(.var)
  check_count_distinct(.data)
  summarize(.data, n = dplyr::n_distinct(!!.var))
}

#' @export
#' @rdname count_distinct
count_distinct_stemid <- function(.data) {
  set_names(.data, tolower) %>%
    check_count_distinct() %>%
    check_crucial_names("stemid") %>%
    # Grouping vars preserve name-case
    count_distinct(.data$stemid)
}

#' @export
#' @rdname count_distinct
abundance_stem <- count_distinct_stemid

#' @export
#' @rdname count_distinct
count_distinct_treeid <- function(.data) {
  set_names(.data, tolower) %>%
    check_count_distinct() %>%
    # Grouping vars preserve name-case
    check_crucial_names("treeid") %>%
    count_distinct(.data$treeid)
}

#' @export
#' @rdname count_distinct
abundance_tree <- count_distinct_treeid

check_count_distinct <- function(.data) {
  if (!is.data.frame(.data)) {
    stop("`.data` must be a dataframe.", call. = FALSE)
  }
  invisible(.data)
}

