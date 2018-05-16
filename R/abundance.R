#' Count individuals by group.
#' 
#' To calculate the abundance of individuals by groups is a particular case of
#' the more general issue of counting observations. This is such a common action
#' that there are great tools for doing just that. Some of the most widely used
#' and well tested are `count()`, `tally()`, `add_count()` and `add_tally()`
#' from the __dplyr__ package. For convenience, those functions are reexported
#' by by __fgeo.abundance__ so they will be available to you when you run
#' `library(fgeo.abundance)`. The examples below are show specifically how these
#' functions apply to ForestGEO data. To learn more about these functions, and
#' to see more general examples, please refer to the original documentation of
#' [dplyr::count()].
#' 
#' @name abundance
#' 
#' @family grouped summaries
#' 
#' @seealso [dplyr::count()].
#' @importFrom dplyr tally count add_tally add_count
#' @export           tally count add_tally add_count
#' @aliases          tally count add_tally add_count
#' 
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
#' # Abundance (rows count) by species
#' count(census, sp)
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
NULL
