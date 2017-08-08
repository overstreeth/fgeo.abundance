# spatial; quadfunc -------------------------------------------------------

validate_gridsize_plotdim <- function(gridsize, plotdim) {
  stopifnot(gridsize > 0)
  stopifnot(all(plotdim > 0))
}

validate_by_position <- function(first_arg, second_arg) {
  stopifnot(assertive::is_non_empty(first_arg))
  stopifnot(assertive::is_non_empty(second_arg))
  
  odd_first_arg <- paste0(substitute(first_arg))
  odd_second_arg <- paste0(substitute(second_arg))
  msg_na_nan <- "and output contain NA or NaN."
  if (anyNA(first_arg)) {warning(paste(odd_first_arg, msg_na_nan))}
  if (anyNA(second_arg)) {warning(paste(odd_second_arg, msg_na_nan))}
  
  msg_inf <- "contains infinite values."
  if (any(is.infinite(first_arg))) {warning(paste(odd_first_arg, msg_inf))}
  if (any(is.infinite(second_arg))) {warning(paste(odd_second_arg, msg_inf))}
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

#' @rdname to_id
#' @export
to_id_rowcol <- function(.row, .col, gridsize, plotdim) {
  validate_by_position(first_arg = .row, second_arg = .col)
  validate_gridsize_plotdim(gridsize = gridsize, plotdim = plotdim)
  
  badrc <- (.row <= 0 | .col <= 0 | .row > plotdim[2] / gridsize |
      .col > plotdim[1] / gridsize)
  diff <- 1
  .row_minus_diff <- .row - diff
  .col_minus_diff <- .col - diff
  maxrow <- floor(plotdim[2] / gridsize)
  index <- .col_minus_diff * maxrow + .row_minus_diff + diff
  if (length(badrc[badrc > 0])) {index[badrc] <- NA}
  return(index)
}

#' @rdname to_id
#' @export
to_id_gxgy <- function(gx, gy, gridsize, plotdim) {
  validate_by_position(first_arg = gx, second_arg = gy)
  validate_gridsize_plotdim(gridsize = gridsize, plotdim = plotdim)
  
  badgxgy <- (gx < 0 | gy < 0 | gx >= plotdim[1] | gy >= plotdim[2] |
        is.na(gx) | is.na(gy))
  .col_adjusted <- 1 + floor(gx / gridsize)
  .row_adjusted <- 1 + floor(gy / gridsize)
  if (length(badgxgy[badgxgy > 0])) {
    .col_adjusted[badgxgy] <- NA
    .row_adjusted[badgxgy] <- NA
  }
  return(to_id_rowcol(.row_adjusted, .col_adjusted, gridsize, plotdim))
}

