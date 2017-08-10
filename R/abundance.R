# spatial; quadfunc -------------------------------------------------------

# Helpers of `to_id_*()` --------------------------------------------------

validate_gridsize_plotdim <- function(gridsize, plotdim) {
  stopifnot(!is.na(gridsize))
  stopifnot(all(!is.na(plotdim)))
  
  stopifnot(gridsize > 0)
  stopifnot(all(plotdim > 0))
}

# Choosingn to warn (not err) because the intention of the function is unclear.
warn_na_rowcol <- function(.row, .col) {
  expr_list <- list(
    quote(is.na(.row)), 
    quote(is.na(.col))
  )
  warn_suspicious <- function(x) {
    if (any(eval(x), na.rm = TRUE)) {
      warning(deparse(x), call. = FALSE)
    }
  }
  lapply(expr_list, warn_suspicious)
}



# Bad arguments, as defined inside `to_id_*()` ----

# These functions throws warning if bad arguments are detected. Errors are not
# appropriate because the bad arguments are meaningful inside these functions.
warn_bad_arg_to_id_gxgy <- function(gx, gy, gridsize, plotdim) {
  expr_list <- list(
    quote(gx < 0),
    quote(gy < 0),
    quote(gx >= plotdim[1]),
    quote(gy >= plotdim[2]),
    quote(is.na(gx)),
    quote(is.na(gy))
  )
  warn_suspicious <- function(x) {
    if (any(eval(x), na.rm = TRUE)) {
      warning(deparse(x), call. = FALSE)
    }
  }
  lapply(expr_list, warn_suspicious)
}

warn_bad_arg_to_id_rowcol <- function(.row, .col, gridsize, plotdim) {
  expr_list <- list(
    quote(.row <= 0),
    quote(.col <= 0),
    quote(.row > plotdim[2] / gridsize),
    quote(.col > plotdim[1] / gridsize)
  )
  warn_suspicious <- function(x) {
    if (any(eval(x), na.rm = TRUE)) {
      warning(deparse(x), call. = FALSE)
    }
  }
  lapply(expr_list, warn_suspicious)
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
  warn_na_rowcol(.row = .row, .col = .col)
  warn_bad_arg_to_id_rowcol(.row = .row, .col = .col, gridsize = gridsize,
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
  validate_gridsize_plotdim(gridsize = gridsize, plotdim = plotdim)
  warn_bad_arg_to_id_gxgy(gx = gx, gy = gy, gridsize = gridsize, 
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



# Basal area --------------------------------------------------------------

#' Basal area of trees.
#'
#' Calculates the individual basal areas (in square meters) for all submitted
#' dbhs. The dbh units must be submitted, either cm'or 'millimeters'.
#' 
#' @inheritParams abundance
#' @template dbh
#' @return A vector of basal area values of same length as the submitted vector 
#'   of dbhs.
'ba'

ba=function(dbh,dbhunit='mm') 
 {
  if(dbhunit=='mm') return(pi*(dbh/2000)^2)
  if(dbhunit=='cm') return(pi*(dbh/200)^2)
 }

#' Returns the basal area summed over all submitted dbhs.
#'
#' @description
#' Returns the basal area summed over all submitted dbhs. NAs can be included,
#' as sum will be completed with `na.rm = TRUE`.
#'
#' @template dbh
#' @template mindbh
#' @template dbhunit
#'
'basum'

basum=function(dbh,mindbh=10,dbhunit='mm')
{
 if(!is.null(mindbh)) dbh=dbh[dbh>=mindbh]

 if(length(dbh)==0) return(0)
 return(sum(ba(dbh,dbhunit=dbhunit),na.rm=TRUE))
}



