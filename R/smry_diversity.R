#' Shannon and Simpson diversity indices.
#' 
#' Adds a column `diversity`, of Shannon and Simpson diversity indices
#' calculated with [vegan::diversity()]. The output is a dataframe, convenient
#' for workflows with general purpose tools such as __dplyr__ and __ggplot2__.
#'
#' @param x A dataframe.
#' @param col A column in `x`.
#' @param index An index as described
#' 
#' @seealso [vegan::diversity()].
#'
#' @return A summary of the input dataframe, with the additional column 
#' `diversity`.
#' @export
#'
#' @examples
#' library(dplyr)
#' library(ggplot2)
#' 
#' cns <- data.frame(
#'   CensusID = factor(rep(c(1, 2), 3)),
#'   quadrat = "0000",
#'   sp = rep(paste0("sp", 1:3), 2),
#'   n = sample.int(100, 6)
#' )
#' cns
#' 
#' smry_diversity(cns, n)
#' 
#' # The output of `smry_diversity` flows well into common pipelines:
#' 
#' diversity <- cns %>%
#'   group_by(CensusID) %>%
#'   smry_diversity(n)
#' # Same
#' diversity <- group_by(cns, CensusID)
#' diversity <- smry_diversity(diversity, n)
#' 
#' diversity
#' 
#' # A plot
#' diversity %>%
#'   ggplot(aes(CensusID, value)) +
#'   geom_col(aes(fill = diversity), position = "dodge")
#' # A summary
#' diversity %>%
#'   group_by(diversity) %>%
#'   summarise(mean_diversity = mean(value))
smry_diversity <- function(x, 
                           col, 
                           index = c("shannon", "invsimpson", "simpson")) {
  col <- rlang::enquo(col)
  col_is_missing <- identical(as.character(col[[2]]), "")
  if (col_is_missing) {
    abort("Argument `col` is missing. Must provide `col`.")
  }

  div <- dplyr::summarise(
    x, diversity = enframe_diversity(x = !! col, index = index)
  )
  tidyr::unnest(tidyr::unnest(div))
}

enframe_diversity <- function(x, index) {
  div <- tibble::enframe(diversity_ls(x, index = index), name = "diversity")
  list(div)
}

diversity_ls <- function(x, index = c("shannon", "invsimpson", "simpson")) {
  purrr::map(.x = index, ~vegan::diversity(x, .x, MARGIN = 2)) %>%
    purrr::set_names(index)
}
