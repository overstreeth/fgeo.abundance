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

#' Count distinct values of any variable (optionally by groups).
#'
#' This function works with grouped data (via
#' `dplyr::group_by()`). The output will have one row for each group.
#'
#' @param .data A dataframe, commonly grouped with `group_by()`. 
#' @param .var A variable to count distinct occurrences.
#' 
#' @family abundance functions.
#' @family functions not specific to ForestGEO
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

#' Count distinct values of treeid, stemid (optionally by groups).
#' 
#' @description
#' Opinions vary on what _abundance_ means. For example, to calculate the
#' abundance of woods with ForestGEO data, you may need to define the `status`
#' and `dbh` range of the stems you are interested. But that's not the job of
#' this functions. These functions only count distinct occurrences of variables.
#' 
#' @description
#' These functions are simpler, specialized versions of `count_distinct()`,
#' specifically for counting distinct occurrences of the variables `stemID` and
#' `treeID` (or `TreeID` and `StemID`) which uniquely identify each stem and
#' tree in ForestGEO-like datasets.
#' 
#' `count_unique_treeid()` throws an error if each data-group contains more
#' than one value of treeid (i.e. if it contains multiple stems). You should
#' first collapse treeid by picking a single stem per treeid per group. Both
#' `count_unique_treeid()` and `count_unique_stemid()` warn if the dataset
#' contains multiple censusid.
#' 
#' @section Warning:
#' These functions do not remove dead stems or trees. If you don't want dead
#' trees to be included, remove them first (see [drop_dead_tree()],
#' [drop_dead_stem()].
#' 
#' @param .data A ForestGEO-like dataframe, commonly grouped with `group_by()`.
#'
#' @family functions for fgeo census and vft.
#'
#' @export
#' 
#' @examples
#' library(dplyr)
#' 
#' census <- tibble::tibble(
#'   treeID = c(1, 1, 2, 3, 3, 3),
#'   stemID = c(1, 2, 3, 4, 5, 6),
#'   quadrat = paste0("000", rep(1:2, each = 3)),
#'   sp = c(paste0("sp", c(1, 1, 2)), paste0("sp", c(3, 3, 3))),
#'   dbh = abs(sample(rnorm(100), 6) * 10)
#' )
#' census
#'
#' by_quad <- group_by(census, quadrat)
#' count_distinct_stemid(by_quad)
#' 
#' count_distinct_treeid(by_quad)
#'
#' by_sp <- group_by(census, sp)
#' count_distinct_stemid(by_sp)
#' 
#' count_distinct_treeid(by_sp)
#'
#' by_quad_sp <- group_by(census, quadrat, sp)
#' count_distinct_stemid(by_quad_sp)
#' count_distinct_treeid(by_quad_sp)
count_distinct_treeid <- function(.data) {
  .x <- set_names(.data, tolower) %>%
    check_count_distinct() %>% 
    groups_lower() %>% 
    check_crucial_names("treeid")
    
    fgeo.tool::flag_duplicated_var(abort, treeid)(.x)
    
    if ("censusid" %in% names(.x)) {
      n_censusid <- dplyr::summarize(.x, n = dplyr::n_distinct(.data$censusid))
      multiple_censusid <- any(n_censusid$n > 1)
      if (multiple_censusid) {
        id <- glue_collapse(
          unique(.x$censusid), sep = ", ", width = 30, last = " and "
        )
        warn(glue("Detected multiple values of censusid: {id}."))
      }
    }
    
    count_distinct(.x, .data$treeid)
}

#' @rdname count_distinct_treeid
#' @export
count_distinct_stemid <- function(.data) {
  .x <- set_names(.data, tolower) %>%
    check_count_distinct() %>% 
    groups_lower() %>% 
    check_crucial_names("stemid")

    if ("censusid" %in% names(.x)) {
      fgeo.base::flag_multiple(.x, "censusid", rlang::warn)
    }
    
  .x %>% 
    check_crucial_names("stemid") %>%
    count_distinct(.data$stemid)
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
