#' Count number of individuals in total or by groups.
#' 
#' Count number of individuals in total (`abundance_tally()`) or by groups
#' (`abundance()`). These functions wrap functions in the __dplyr__ package to
#' provide shortcuts similar to those in the original CTFSRPackage (see
#' Warning).
#' 
#' @section Warning:
#' You should prefer using dplyr dirctly (see Examples) -- it is more explicit
#' and general, which will make your code more readable by people outside
#' ForestGEO.
#'
#' @template  x
#' @template group_by
#' @template only_alive
#'
#' @return
#'   * `abundance_tally()`: An integer giving the number of rows in `x`, which 
#'     for census data equals the number of unique individuals.
#'   * `abundance()`: a data frame giving the number of individuals in each 
#'     group defined by `group_by`.
#'     
#' @export
#' @author Gabriel Arellano, Sean McMahon & Mauro Lepore.
#' @seealso [dplyr::tally()], [dplyr::count()].
#' @family grouped summaries
#' @examples
#' stem <- fgeo.data::luquillo_stem_random_tiny
#' 
#' # Returns a data frame by groups
#' n <- abundance(stem)
#' head(n)
#' 
#' # Instead of a data frame, return an integer representing a tally
#' abundance_tally(stem)
#' 
#' # Count not only alive
#' n <- abundance(stem, group_by = c("status", "sp"), only_alive = FALSE)
#' ordered_by_species <- order(n$sp)
#' head(n[ordered_by_species, ])
#' 
#' 
#' 
#' # ALTERNATIVE: Prefer dplyr; it is more general and explicit
#' library(dplyr)
#' 
#' alive <- dplyr::filter(stem, status == "A")
#' grouped <- count(alive, quadrat, sp)
#' 
#' census <- tibble(
#'   stemID = 1:6,
#'   quadrat = paste0("000", rep(1:2, each = 3)),
#'   sp = c(paste0("sp", c(1, 1, 2)), paste0("sp", c(3, 3, 3))),
#'   dbh = abs(sample(rnorm(100), 6) * 10)
#' )
#' census
#' 
#' # Abundance (rows count) by quadrat
#' count(census, quadrat)
#' # same
#' census %>% count(quadrat)
#' 
#' # Abundance (rows count) by species
#' count(census, sp)
#' 
#' # Abundance (rows count) by quadrat by species
#' count(census, quadrat, sp)
#' # same
#' census %>% count(quadrat, sp)
#' 
#' 
#' # Richness by quadrat:
#' # Count is designed so that you can call it repeatedly, each time rolling up a
#' # level of detail.
#' # Now, each row by quadrat is a unique species, so counting rows gives 
#' # richness
#' census %>% 
#'   count(quadrat, sp) %>% 
#'   count(quadrat)
#' 
#' # Singleton:
#' # add_count() is useful for groupwise filtering e.g.: show only species that
#' # have a single member
#' census %>% 
#'   add_count(quadrat, sp) %>% 
#'   filter(n == 1)
abundance <- function(x, group_by = c("quadrat", "sp"), only_alive = TRUE) {
  grouped <- group(x = x, group_by = group_by, only_alive = only_alive)
  count <- dplyr::summarise(grouped, n = n())
  as.data.frame(count, stringsAsFactors = FALSE)
}

#' @rdname abundance
#' @export
abundance_tally <- function(x, only_alive = TRUE) {
  if (only_alive) {
    x <- x[x$status == "A", ]
  }
  dim(x)[1L]
}


