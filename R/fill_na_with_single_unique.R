# TODO: Move to fgeo.base

#' Replace missing values with single occurrences of a vector or die trying.
#' 
#'
#' @param x A vector.
#'
#' @seealso `rlang::`%|%`.
#' 
#' @return The input vector with missing values replaced by unique ocurrences
#'   of x.
#'   
#' @export
#'
#' @examples
#' 
#' fill_na_with_single_unique(c(1, NA))
#' fill_na_with_single_unique(c("a", NA))
#' fill_na_with_single_unique(c(TRUE, NA))
#' 
#' # Fails
#' # fill_na_with_single_unique(c(1, 2, NA))
fill_na_with_single_unique <- function(x) {
  if (length(unique(na.omit(x))) != 1) {
    stop(
      "Unique non-missing values of `x` must be of length 1 not ", 
      length(unique(na.omit(x))), ".",
      call. = FALSE
    )
  }
  
  fgeo.base::fill_na(x, filler = na.omit(x))
}
