#' Abundance, richness, singletons: Count individuals in total or by groups.
#' 
#' * `count()`, `tally()`, `add_count()` and `add_tally()` help you count rows in
#' a dataframe. They come from the __dplyr__ package and are reexported by 
#' __fgeo.abundance__ for your convenience (see `reexports`). For details see
#' [dplyr::count()].
#' * `count_duplicated` helps you count duplicated observations of a variable. It
#' is a shortcut for `dplyr::filter(dplyr::count(x, ...) > 1)`).
#' 
#' The issue of calculating the abundance of individuals in a sample is a
#' particular case of counting observations in general. By counting individuals
#' you can also calculate richness, find singletons, or duplicated observations
#' of a variable. The examples below show how the most commonly used functions
#' may be applied to ForestGEO data. To learn more about these functions, and to
#' see more general examples see `?dplyr::count()`.
#' 
#' All of these functions quote some argument to make interactive use easier. If
#' you want to use them inside your own functions you should learn about tidy
#' eval (implemented via the __rlang__ package). A good place to start is at
#' __dplyr__'s website.
#' 
#' @param x A dataframe.
#' @inheritParams dplyr::count
#'
#' @return A dataframe.
#' 
#' @aliases abundance
#' @seealso [dplyr::count()].
#' 
#' @family grouped summaries
#' 
#' @export
#' @examples
#' census <- tibble::tibble(
#'   stemID = 1:6,
#'   quadrat = paste0("000", rep(1:2, each = 3)),
#'   sp = c(paste0("sp", c(1, 1, 2)), paste0("sp", c(3, 3, 3))),
#'   dbh = abs(sample(rnorm(100), 6) * 10)
#' )
#' census
#' 
#' # Abundance (rows count) by quadrat
#' count(census, quadrat)
#' 
#' # Abundance (rows count) by quadrat by species
#' count(census, quadrat, sp)
#' 
#' # Richness by quadrat:
#' # Count is designed so that you can call it repeatedly, each time rolling up
#' # a level of detail.
#' # Now, each row by quadrat is a unique species, so counting rows gives
#' # richness
#' n_quad_sp <- count(census, quadrat, sp)
#' richness_quad <- count(n_quad_sp, quadrat)
#' richness_quad
#' 
#' # Singleton:
#' # add_count() is useful for groupwise filtering e.g.: show only species that
#' # have a single member
#' out <- add_count(census, quadrat, sp)
#' subset(out, n == 1)
#' # Or the complement
#' subset(out, n > 1)
#' 
#' # A shortcut to find duplicates that drops irrelevant columns:
#' count_duplicated(census, quadrat, sp)
#' # Any duplicated stemID?
#' count_duplicated(census, stemID)
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
