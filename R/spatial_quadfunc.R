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
  validate_to_id_rowcol(.row = .row, .col = .col, gridsize = gridsize,
    plotdim = plotdim)
  
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
  validate_to_id_gxgy(gx = gx, gy = gy, gridsize = gridsize, 
  plotdim = plotdim)

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



# validate ----------------------------------------------------------------

validate_to_id_rowcol <- function(.row, .col, gridsize, plotdim) {
  validate_gridsize_plotdim(gridsize = gridsize, plotdim = plotdim)
  validate_row_col(.row = .row, .col = .col)
  warn_bad_arg_to_id_rowcol(.row = .row, .col = .col, gridsize = gridsize,
    plotdim = plotdim)
}

validate_to_id_gxgy <- function(gx, gy, gridsize, plotdim){
  validate_gridsize_plotdim(gridsize = gridsize, plotdim = plotdim)
  warn_bad_arg_to_id_gxgy(gx = gx, gy = gy, gridsize = gridsize, 
  plotdim = plotdim)
}

validate_gridsize_plotdim <- function(gridsize, plotdim) {
  assertive::assert_all_are_not_na(gridsize)
  assertive::assert_all_are_not_na(plotdim)
  assertive::assert_all_are_positive(gridsize)
  assertive::assert_all_are_positive(plotdim)
}

validate_row_col <- function(.row, .col) {
  assertive::assert_all_are_not_na(.row, severity = "warning")
  assertive::assert_all_are_not_na(.col, severity = "warning")
}

# Functions warn_bad_arg*() warn if bad arguments are detected. Errors are not
# appropriate because the bad arguments are meaningful inside these functions.

warn_bad_arg_to_id_gxgy <- function(gx, gy, gridsize, plotdim) {
  assertive::assert_all_are_positive(gx, severity = "warning")
  assertive::assert_all_are_positive(gy, severity = "warning")
  assertive::assert_all_are_false(
    gx >= plotdim[1], severity = "warning"
  )
  assertive::assert_all_are_false(
    gy >= plotdim[2], severity = "warning"
  )
  assertive::assert_all_are_not_na(gx, severity = "warning")
  assertive::assert_all_are_not_na(gy, severity = "warning")
}

warn_bad_arg_to_id_rowcol <- function(.row, .col, gridsize, plotdim) {
  assertive::assert_all_are_non_negative(.row, severity = "warning")
  assertive::assert_all_are_non_negative(.col, severity = "warning")
  assertive::assert_all_are_false(
    .row > plotdim[2] / gridsize, severity = "warning"
  )
  assertive::assert_all_are_false(
    .col > plotdim[1] / gridsize, severity = "warning"
  )
}


