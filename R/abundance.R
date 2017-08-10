# spatial; quadfunc -------------------------------------------------------

# Helpers of `to_id_*()` --------------------------------------------------

validate_gridsize_plotdim <- function(gridsize, plotdim) {
  stopifnot(gridsize > 0)
  stopifnot(all(plotdim > 0))
}


# Helper to warn for odd arguments
warn_suspicious <- function(.x, msg = NULL) {
  if (any(eval(.x), na.rm = TRUE)) {
    warning(msg, deparse(.x), call. = FALSE)
    }
}

# These functions throws warning if bad arguments are detected. Errors are not
# appropriate because the bad arguments are meaningful inside these functions.
warn_bad_arg_to_id_gxgy <- function(gx, gy, gridsize, plotdim, ...) {
  substituted_list <- list(
    substitute(gx < 0),
    substitute(gy < 0),
    substitute(gx >= plotdim[1]),
    substitute(gy >= plotdim[2]),
    substitute(is.na(gx)),
    substitute(is.na(gy))
  )
  lapply(substituted_list, warn_suspicious, ...)
}

warn_bad_arg_to_id_rowcol <- function(.row, .col, gridsize, plotdim, ...) {
  substituted_list <- list(
    substitute(.row <= 0),
    substitute(.col <= 0),
    substitute(.row > plotdim[2] / gridsize),
    substitute(.col > plotdim[1] / gridsize)
  )
  lapply(substituted_list, warn_suspicious, ...)
}



# `to_id_*()` -------------------------------------------------------------

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
  validate_gridsize_plotdim(gridsize = gridsize, plotdim = plotdim)
  warn_bad_arg_to_id_rowcol(.row = .row, .col = .col, gridsize = gridsize,
    plotdim = plotdim, msg = "Some bad arguments were detected: ")
  
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
  validate_gridsize_plotdim(gridsize = gridsize, plotdim = plotdim)
  warn_bad_arg_to_id_gxgy(gx = gx, gy = gy, gridsize = gridsize, 
  plotdim = plotdim, msg = "Some bad arguments were detected: ")
  
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

