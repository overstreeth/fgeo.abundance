# spatial; quadfunc -------------------------------------------------------


validate_args_to_id_rowcol <- function(.row, .col, gridsize, plotdim) {
  stopifnot(gridsize > 0)
  stopifnot(all(plotdim > 0))
  stopifnot(assertive::is_non_empty(.row))
  stopifnot(assertive::is_non_empty(.col))
  odd_row <- paste0(substitute(.row))
  odd_col <- paste0(substitute(.col))
  msg_na_nan <- "and output contain NA or NaN."
  if (anyNA(.row)) {warning(paste(odd_row, msg_na_nan))}
  if (anyNA(.col)) {warning(paste(odd_row, msg_na_nan))}
  msg_inf <- "contains infinite values."
  if (any(is.infinite(.row))) {warning(paste(odd_row, msg_inf))}
  if (any(is.infinite(.col))) {warning(paste(odd_col, msg_inf))}
}

#' Takes row and column numbers and identifies the quadrate number (index).
#'
#' @description
#' From row and column numbers (based on `gridsize`) outputs the corresponding
#' quadrat number, i.e. index (id).
#' 
#' @param .row Row number.
#' @param .col Column number.
#' @template gridsize
#' @template plotdim
#' 
#' @aliases rowcol.to.index
#' @family converters
#' @return A numeric vector of indices.
#' 
#' @export
#'
#' @examples
#' to_id_rowcol(.row = 1:10, .col = 1:10, gridsize = 20, plotdim = c(1000, 500))
to_id_rowcol <- function(.row, .col, gridsize, plotdim) {
  validate_args_to_id_rowcol(.row = .row, .col = .col, gridsize = gridsize, 
    plotdim = plotdim)
  
  badrc = (.row <= 0 | .col <= 0 | .row > plotdim[2]/gridsize | 
    .col > plotdim[1]/gridsize)
  .row = .row - 1
  .col = .col - 1
  maxrow = floor(plotdim[2]/gridsize)
  index = .col * maxrow + .row + 1
  if (length(badrc[badrc > 0])) 
    index[badrc] = NA
  return(index)
}
