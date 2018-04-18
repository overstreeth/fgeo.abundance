#' Shannon and Simpson diversity indices.
#' 
#' Adds a column `diversity`, of Shannon and Simpson diversity indices
#' calculated with [vegan::diversity()]. The output is a dataframe, convenient
#' for workflows with general purpose tools such as __dplyr__ and __ggplot2__.
#'
#' @param x A dataframe.
#' @param abundance A numeric column in `x` giving the abundance by species.
#' @param index An index as described
#' 
#' @seealso [vegan::diversity()].
#'
#' @return A summary of the input dataframe, with the additional column 
#' `diversity`.
#' 
#' @section Acknowledgements:
#' * David Kenfack inspired the need for this function. 
#' * A talk by Jenny Bryan (rstd.io/row-work) inspired the fundamental
#'   implementation details.
#' 
#' @export
#'
#' @examples
#' library(dplyr)
#' library(ggplot2)
#' 
#' census <- data.frame(
#'   quadrat = rep(c("0000", "0001"), each = 3),
#'   sp = rep(paste0("sp", 1:3), 2),
#'   n = sample.int(100, 6),
#'   stringsAsFactors = FALSE
#' )
#' census
#' 
#' ungrouped <- census
#' vgn_diversity(ungrouped, n)
#' 
#' by_quadrat <- group_by(census, quadrat)
#' vgn_diversity(by_quadrat, n)
#' 
#' vgn_diversity(by_quadrat, n, index = "shannon")
#' 
#' # The output of `vgn_diversity` flows well into common pipelines:
#' 
#' diversity <- census %>%
#'   group_by(quadrat) %>%
#'   vgn_diversity(n)
#' diversity
#' 
#' # A plot
#' diversity %>%
#'   ggplot(aes(quadrat, value)) +
#'   geom_col(aes(fill = index), position = "dodge")
#' 
#' # A summary
#' diversity %>%
#'   group_by(index) %>%
#'   summarise(mean = mean(value))
vgn_diversity <- function(x, 
                          abundance, 
                          index = c("shannon", "invsimpson", "simpson")) {
  stopifnot(is.data.frame(x))
  abundance <- rlang::enquo(abundance)
  abundance_is_missing <- identical(as.character(abundance[[2]]), "")
  if (abundance_is_missing) {
    abort("Argument `abundance` is missing. Must provide `abundance`.")
  }

  div <- dplyr::summarise(
    x, diversity = enframe_diversity(x = !! abundance, index = index)
  )
  tidyr::unnest(tidyr::unnest(div))
}

enframe_diversity <- function(x, index) {
  div <- tibble::enframe(diversity_ls(x, index = index), name = "index")
  list(div)
}

diversity_ls <- function(x, index = c("shannon", "invsimpson", "simpson")) {
  purrr::map(.x = index, ~vegan::diversity(x, .x, MARGIN = 2)) %>%
    purrr::set_names(index)
}
