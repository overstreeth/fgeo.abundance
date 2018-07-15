#' Abundance of trees, stems, and count distinct values of a variable by groups.
#'
#' These functions count rows by groups (if you need to exclude some
#' observations, to it first -- see warning). These functions are usually used
#' on grouped data created by `dplyr::group_by()` (reexported by __fgeo__). The
#' output will have one row for each group:
#' * `count_distinct()` counts distinct occurrences of any variable of any
#' dataset (not specifically a ForestGEO dataset).
#' * `count_distinct_stemid()` and `count_distinct_treeid()` are simpler and
#' specialized versions `count_distinct()`, for specifically counting distinct
#' occurrences of the variables `TreeID` and `StemID`, or `stemID` and `treeID`,
#' which uniquely identify each stem and tree in `ViewFullTable`s and census
#' datasets of ForestGEO.
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
#' count_distinct(mtcars)
#' count_distinct(mtcars, cyl)
#' dplyr::n_distinct(mtcars)
#' 
#' \dontrun{
#' # Fails because `...` to `n_distinct()` are vectors, not dataframe-variables
#' dplyr::n_distinct(mtcars, cyl)
#' }
#' 
#' 
#' length(unique(mtcars$mpg))
#' 
#' 
#' 
#' count_distinct(group_by(mtcars, cyl), mpg)
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
#' 
#' count_distinct_treeid(by_quad)
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
  
  tryCatch(
    result <- summarize(.data, n = dplyr::n_distinct(!!.var)), 
    error = function(e) {
      if (rlang::quo_is_missing(.var)) {
        stop("`.var` must be supplied but it's missing.", call. = FALSE)
      }
      
      conditionMessage(e)
    }
  )
  
  .var <- pull_name(rlang::expr_name(rlang::get_expr(.var)))
  if (!.var %in% names(.data)) {
    stop("`.var` must be a variable of `.data` but it's not.", call. = TRUE)
  }
  
  result
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
count_distinct_treeid <- function(.data) {
  set_names(.data, tolower) %>%
    check_count_distinct() %>%
    # Grouping vars preserve name-case
    check_crucial_names("treeid") %>%
    count_distinct(.data$treeid)
}

check_count_distinct <- function(.data) {
  if (!is.data.frame(.data)) {
    stop("`.data` must be a dataframe.", call. = FALSE)
  }
  
  invisible(.data)
}

