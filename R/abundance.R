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
#' @param .var A variable to count distinct occurances.
#' 
#' @seealso [dplyr::group_by()], [dplyr::summarise()].
#' 
#' @return An object of the same class as .data. One grouping level will be
#'   dropped.
#' @export
#'
#' @examples
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
  count_distinct(.data, .data$stemID)
}

#' @export
#' @rdname count_distinct
abundance_tree <- function(.data) {
  count_distinct(.data, .data$treeID)
}










#' Count individuals in total or by groups (abundance, richness, singletons).
#' 
#' These functions help you to count rows in a dataframe:
#' * `abundance()` is an identical copy of `dplyr::count()`, and
#' `add_abundance()` is an identical copy of `dplyr::add_count()`.
#' * `count_duplicated` helps you count duplicated observations of a variable.
#' It is a shortcut for `dplyr::filter(dplyr::count(x, ...) > 1)`).
#' * Also reexported from __dplyr__ are `tally()` and `add_tally()`. For details
#' see [dplyr::count()].
#' 
#' The issue of calculating the abundance of individuals in a sample is a
#' particular case of counting observations in general. By counting individuals
#' you can also calculate richness, find singletons, or duplicated observations
#' of a variable. The examples below show how the most commonly used functions
#' may be applied to ForestGEO data. To learn more about these functions, and to
#' see more general examples see `?dplyr::count()`.
#' 
#' All of these functions quote some argument to make interactive use easier.
#' This mean that you can refer to columns of the data directly by their bare
#' name (not using "quotes"). If you want to use them inside your own functions
#' you should learn about tidy eval (implemented via the __rlang__ package). A
#' good place to start is at __dplyr__'s website.
#' 
#' @param x A dataframe.
#' @inheritParams dplyr::count
#'
#' @return A dataframe.
#' 
#' @seealso [dplyr::count()].
#' 
#' @family grouped summaries
#' 
#' @section Acknowledgments:
#' Thanks to David Kenfack for sharing ideas that helped improve these 
#' functions.
#' 
#' @examples
#' # Example data
#' census <- tibble::tibble(
#'   stemID = 1:6,
#'   quadrat = paste0("000", rep(1:2, each = 3)),
#'   sp = c(paste0("sp", c(1, 1, 2)), paste0("sp", c(3, 3, 3))),
#'   dbh = abs(sample(rnorm(100), 6) * 10)
#' )
#' census
#' 
#' # Abundance (rows count) by quadrat
#' abundance(census, quadrat)
#' # Identical
#' count(census, quadrat)
#' 
#' # Abundance (rows count) by quadrat by species
#' abundance(census, quadrat, sp)
#' 
#' # Richness by quadrat:
#' # Count is designed so that you can call it repeatedly, each time rolling up
#' # a level of detail.
#' # Now, each row by quadrat is a unique species, so counting rows gives
#' # richness
#' n_quad_sp <- abundance(census, quadrat, sp)
#' richness_quad <- abundance(n_quad_sp, quadrat)
#' richness_quad
#' 
#' # Singleton:
#' # add_abundance() is useful for groupwise filtering e.g.: show only species 
#' # that have a single member
#' out <- add_abundance(census, quadrat, sp)
#' # Identical
#' out <- add_count(census, quadrat, sp)
#' subset(out, n == 1)
#' # Or the complement
#' subset(out, n > 1)
#' 
#' # A shortcut to find duplicates that drops irrelevant columns:
#' count_duplicated(census, quadrat, sp)
#' # Any duplicated stemID?
#' count_duplicated(census, stemID)
#' @name abundance
NULL

#' @rdname abundance
#' @export
abundance <- dplyr::count

#' @rdname abundance
#' @export
add_abundance <- dplyr::add_count

#' @rdname abundance
#' @export
count_duplicated <- function(x, ..., wt = NULL, sort = FALSE) {
  if (length(dplyr::group_vars(x)) != 0) {
    warn("Can't handle grouped `x`. Ungrouping to continue.")
    x <- dplyr::ungroup(x)
  }
  
  group_vars <- enquos(...)
  count_call <- quo(dplyr::count(x, !!!group_vars, wt = !!wt, sort = !!sort))
  cnt <- rlang::eval_tidy(count_call, x)
  dplyr::filter(cnt, dplyr::last(cnt) > 1)
}

