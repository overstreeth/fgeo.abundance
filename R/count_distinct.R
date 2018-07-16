#' Count distinct values of treeid, stemid, or any variable by groups.
#'
#' @description
#' Opinions vary on what _abundance_ means. For example, to calculate the
#' abundance of woods with ForestGEO data, you may need to define the `status`
#' and `dbh` range of the stems you are interested. But that's not the job of
#' this functions. These functions only count distinct occurrences of variables.
#' Thus, to calculate abundance in the exact way you want you need to compose
#' these functions (e.g. `collapse_treeid_max()`, `pick_dbh_min()`,
#' `pick_status()`, `drop_dead_tree()`). To avoid creating false expectations, the
#' name of these functions intentionally avoid the word _abundance_.
#' 
#' @description
#' These functions are usually used with grouped data (via
#' `dplyr::group_by()`). The output will have one row for each group.
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
#' @family abundance functions.
#' @family functions for fgeo census and vft.
#' 
#' @seealso [dplyr::group_by()], [dplyr::summarise()], [drop_dead_tree()],
#' [drop_dead_stem()].
#'
#' @return An object of the same class as .data. One grouping level will be
#'   dropped.
#' @export
#'
#' @examples
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
#' 
#' # Compare to dplyr::summarize() and dplyr::n_distinct()
#' library(dplyr)
#' 
#' by_cyl <- group_by(mtcars, cyl)
#' count_distinct(by_cyl, mpg)
#' # Same
#' summarize(by_cyl, n = n_distinct(mpg))
#' 
#' n_distinct(mtcars)
#' # Same
#' nrow(mtcars)
#' 
#' \dontrun{
#' # Fails; you must supply `.var` to count distinct values if that variable
#' count_distinct(mtcars)
#' }
#' 
#' count_distinct(mtcars, cyl)
#' # Same
#' n_distinct(mtcars$cyl)
#' # Same
#' length(unique(mtcars$cyl))
#' 
#' \dontrun{
#' # Fails because it expect not column names but additional vectors (via `...`)
#' dplyr::n_distinct(mtcars, cyl)
#' }
count_distinct <- function(.data, .var) {
  .var <- enquo(.var)
  count_distinct_impl(.data, .var)
}

#' @export
#' @rdname count_distinct
count_distinct_stemid <- function(.data) {
  set_names(.data, tolower) %>%
    check_count_distinct() %>%
    check_crucial_names("stemid") %>%
    count_distinct(.data$stemid)
}

#' @export
#' @rdname count_distinct
count_distinct_treeid <- function(.data) {
  set_names(.data, tolower) %>%
    check_count_distinct() %>%
    check_crucial_names("treeid") %>%
    count_distinct(.data$treeid)
}

count_distinct_impl <- function(.data, .var) {
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
  
  if (invalid_var(.data, .var)) {
    stop("`.var` must be a variable of `.data` but it's not.", call. = TRUE)
  }
  
  result
}

check_count_distinct <- function(.data) {
  if (!is.data.frame(.data)) {
    stop("`.data` must be a dataframe.", call. = FALSE)
  }
  
  invisible(.data)
}

invalid_var <- function(.data, .var) {
  .var <- pull_name(rlang::expr_name(rlang::get_expr(.var)))
  !.var %in% names(.data)
}
