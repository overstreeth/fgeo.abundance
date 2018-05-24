#' Diversity metrics from vegan.
#'
#' Calculates common diversity metrics documented in [vegan::diversity()].
#' Compared to the output of __vegan__, these functions output a dataframe
#' conveniently structure for workflows with general purpose tools such as
#' __dplyr__ and __ggplot2__.
#'
#' @param x A dataframe.
#' @param abundance A numeric column in `x` giving the abundance by species.
#' @param index Some of c("specnumber", "shannon", "invsimpson", "simpson").
#'
#' @seealso [vegan::diversity()].
#'
#' @return A summary of the input dataframe, with the additional column
#' `diversity`.
#'
#' @section Acknowledgments:
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
#'   sp = paste0("sp", c(1:3, 1, 4, 5)),
#'   n = sample.int(100, 6),
#'   stringsAsFactors = FALSE
#' )
#' census <- census[1:5, ]
#' census
#'
#' ungrouped <- census
#' vgn_diversity(ungrouped, n)
#'
#' by_quadrat <- group_by(census, quadrat)
#' vgn_diversity(by_quadrat, n)
#'
#' # Similar alternative
#' summarise(
#'   by_quadrat,
#'   specnumber = vegan::specnumber(n),
#'   shannon = vegan::diversity(n)
#' )
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
vgn_diversity <- function(x, abundance, index = c("specnumber", "shannon")) {
  stopifnot(is.data.frame(x))
  abundance <- rlang::enquo(abundance)
  abundance_is_missing <- identical(as.character(abundance[[2]]), "")
  if (abundance_is_missing) {
    abort("Argument `abundance` is missing. Must provide `abundance`.")
  }

  div <- dplyr::summarise(
    x,
    diversity = enframe_diversity(x = !!abundance, index = index)
  )
  tidyr::unnest(tidyr::unnest(div))
}

enframe_diversity <- function(x, index) {
  div <- combine_diversity_indices(x, index = index) %>%
    tibble::enframe(name = "index")
  list(div)
}

combine_diversity_indices <- function(x, index = c("specnumber")) {
  check_indices(index)
  out <- c(vegan_specnumber(x, index), vegan_diversity(x, index))
  out[!is.na(out)]
}

check_indices <- function(index) {
  good_idx <- c("specnumber", "shannon", "invsimpson", "simpson")
  some_invalid_idx <- !all(index %in% good_idx)
  if (some_invalid_idx) {
    abort(paste0("Invalid `index` (valid: ", good_idx, ")"))
  }
  invisible()
}

vegan_specnumber <- function(x, index) {
  out <- ifelse(
    "specnumber" %in% index,
    vegan::specnumber(x, MARGIN = 1),
    NA
  )
  c(specnumber = out)
}

vegan_diversity <- function(x, index) {
  div_index <- setdiff(index, "specnumber")
  out <- if (length(div_index) > 0) {
  out <- lapply(div_index, function(id) vegan::diversity(x, id, MARGIN = 2))
  rlang::set_names(out, div_index)
  } else {
    NA
  }
  out
}
