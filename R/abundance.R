# spatial; quadfunc -------------------------------------------------------

validate_gridsize_plotdim <- function(gridsize, plotdim) {
  stopifnot(gridsize > 0)
  stopifnot(all(plotdim > 0))
}

validate_args_to_id_rowcol <- function(.row, .col) {
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

#' Convert to quadrat indices.
#' 
#' These functions output quadrat indices from either row and column numbers
#' ([to_id_rowcol]), or gx and gy coordinates ([to_id_gxgy]]). They replace
#' [ctfs::rowcol.to.index] and [ctfs::gxgy.to.index], but these old names are 
#' kept as aliases to help users find the newer names.
#' 
#' @param .row,.col Row and column number.
#' @template gx_gy
#' @template gridsize
#' @template plotdim
#' 
#' @return A numeric vector of indices.
#' 
#' 
#' @aliases rowcol.to.index gxgy.to.index
#' @template author_condit
#' @family converter functions
#' @name to_id
#' @examples
#' library(dplyr)
#' mini_cns <- bci::bci12full7 %>% 
#'   select(gx, gy) %>% 
#'   sample_n(10)
#' to_id_gxgy(mini_cns$gx, mini_cns$gy, gridsize = 20, plotdim = c(1000, 500))
#' 
#' to_id_rowcol(.row = 1:10, .col = 6:15, gridsize = 20, plotdim = c(1000, 500))
NULL

#' @rdname to_id_rowcol
#' @export
to_id_rowcol <- function(.row, .col, gridsize, plotdim) {
  validate_args_to_id_rowcol(.row = .row, .col = .col)
  validate_gridsize_plotdim(gridsize = gridsize, plotdim = plotdim)
  
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

#' @rdname to_id_gxgy
#' @export
to_id_gxgy <- function(gx, gy, gridsize, plotdim) {
  validate_gridsize_plotdim(gridsize = gridsize, plotdim = plotdim)

  badgxgy = (gx < 0 | gy < 0 | gx >= plotdim[1] | gy >= plotdim[2] |
      is.na(gx) | is.na(gy))
  .col = 1 + floor(gx / gridsize)
  .row = 1 + floor(gy / gridsize)
  if(length(badgxgy[badgxgy > 0])) {
    .col[badgxgy] = .row[badgxgy] = NA
  }
  return(to_id_rowcol(.row, .col, gridsize, plotdim))
}

