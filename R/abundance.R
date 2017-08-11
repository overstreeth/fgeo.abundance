# spatial; quadfunc -------------------------------------------------------

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



# `to_id_*()`, validate arguments -----------------------------------------

validate_gridsize_plotdim <- function(gridsize, plotdim) {
  assertive::assert_all_are_not_na(gridsize)
  assertive::assert_all_are_not_na(plotdim)
  assertive::assert_all_are_positive(gridsize)
  assertive::assert_all_are_positive(plotdim)
}

# Choosingn to warn (not err) because the intention of the function is unclear.
warn_na_rowcol <- function(.row, .col) {
  assertive::assert_all_are_not_na(.row, severity = "warning")
  assertive::assert_all_are_not_na(.col, severity = "warning")
}

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
      warning(deparse(x), call. = TRUE)
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
      warning(deparse(x), call. = TRUE)
    }
  }
  lapply(expr_list, warn_suspicious)
}



# Basal area --------------------------------------------------------------

#' Basal area of trees.
#' 
#' Calculates the individual basal areas in square meters for all `dbh`-values
#' submitted.
#' 
#' @template dbhunit
#' @template dbh
#' @return A vector of basal area values of same length as the submitted vector 
#'   of dbhs.
#' @export
#' @examples
#' dbh <- c(0, 23, 43)
#' expect_equal(ba(dbh = dbh)
ba <- function(dbh, dbhunit = "mm") {
  validate_dbh_dbhunit(dbh, dbhunit)

  if (dbhunit == "cm") {return(pi * (dbh / 200) ^ 2)}
  pi * (dbh / 2000) ^ 2
}

#' Basal area summed over all submitted dbhs.
#' 
#' Calculates the basal area summed over all submitted `dbh` values (after
#' removing NAs).
#' 
#' @template dbh
#' @template mindbh
#' @template dbhunit
#'   
#' @return A number giving the the basal area summed over all submitted dbhs.
#' @export
#' @examples 
#' basum(dbh = c(1, 23, NA), mindbh = 23, dbhunit = "cm")
basum <- function(dbh, mindbh = 10, dbhunit = "mm") {
  validate_dbh_dbhunit(dbh, dbhunit)
  assertive::assert_is_a_number(mindbh, severity = "warning")
  assertive::assert_all_are_positive(mindbh)
  
  if (!is.null(mindbh)) {
    dbh <- dbh[dbh >= mindbh]
  }
  if (length(dbh) == 0) {
    return(0)
  }
  sum(ba(dbh, dbhunit = dbhunit), na.rm = TRUE)
}



# Basal area, validate arguments ------------------------------------------

validate_dbh_dbhunit <- function(dbh, dbhunit) {
  assertive::assert_is_numeric(dbh)
  assertive::assert_any_are_matching_regex(dbhunit, pattern = "^mm$|^cm$")
  assertive::assert_all_are_positive(dbh, na_ignore = TRUE)
  assertive::assert_all_are_not_na(dbh, severity = "warning")
}
