#' Count number of individuals in total or by groups.
#' 
#' Count number of individuals in total (`abundance_tally()`) or by groups 
#' (`abundance()`).
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
#' stem <- bci12s7mini
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
#' # ALTERNATIVES
#' 
#' library(dplyr)
#' 
#' alive <- filter(stem, status == "A")
#' 
#' grouped <- group_by(alive, quadrat, sp)
#' summarise(grouped, n = n())
#' 
#' # Or
#' count(alive, quadrat, sp)
#' @export
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


