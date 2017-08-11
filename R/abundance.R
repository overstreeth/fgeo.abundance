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
  validate_row_col(.row = .row, .col = .col)
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
validate_row_col <- function(.row, .col) {
  assertive::assert_all_are_not_na(.row, severity = "warning")
  assertive::assert_all_are_not_na(.col, severity = "warning")
}

# These functions throws warning if bad arguments are detected. Errors are not
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



# Basal area --------------------------------------------------------------

#' Basal area of trees.
#' 
#' * `ba()`: calculates the individual basal areas in square meters for all
#'   `dbh`-values submitted.
#' * `ba_sum()`: calculates the basal area summed over all submitted `dbh` values
#'   (after removing NAs).
#' 
#' @template dbh
#' @template dbhunit
#' @template mindbh
#' 
#' @template author_condit
#' 
#' @return
#'   * `ba()`: A vector of basal area values of same length as the submitted 
#'     vector of dbhs.
#'   * `ba_sum()`: A number giving the the basal area summed over all submitted
#'     dbhs.
#'
#' @name basal_area
#' @aliases ba_sum
#'
#' @examples 
#' # Takes NAs and throws informative warning
#' (dbh <- c(seq(5, 50, by = 5), NA))
#' ba(dbh, dbhunit = "cm")  # default dbhunit is milimeters ("mm")
#' 
#' # (From now on, suppressing warnings to avoid distraction)
#' 
#' # Filter stems at or over a minimum dbh and sum them, all in one step
#' suppressWarnings(
#'   ba_sum(dbh, mindbh = 30)
#' )
#' 
#' # Same; a bit longer but may be easier to understand what is going on
#' over_mindbh <- dbh[dbh >= 30]
#' suppressWarnings(
#'   sum(ba(over_mindbh), na.rm = TRUE)
#' )

#' @rdname basal_area
#' @export
ba <- function(dbh, dbhunit = "mm") {
  validate_dbh_dbhunit_mindbh(dbh = dbh, dbhunit = dbhunit)

  if (dbhunit == "cm") {return(pi * (dbh / 200) ^ 2)}
  pi * (dbh / 2000) ^ 2
}

#' @rdname basal_area
#' @export
ba_sum <- function(dbh, dbhunit = "mm", mindbh = 10) {
  validate_dbh_dbhunit_mindbh(dbh = dbh, dbhunit = dbhunit, mindbh = mindbh)
  
  if (!is.null(mindbh)) {
    dbh <- dbh[dbh >= mindbh]
  }
  if (length(dbh) == 0) {
    return(0)
  }
  sum(ba(dbh, dbhunit = dbhunit), na.rm = TRUE)
}



# Basal area, validate arguments ------------------------------------------

validate_dbh_dbhunit_mindbh <- function(dbh, dbhunit, mindbh = NULL) {
  assertive::assert_is_numeric(dbh)
  assertive::assert_any_are_matching_regex(dbhunit, pattern = "^mm$|^cm$")
  assertive::assert_all_are_positive(dbh, na_ignore = TRUE)
  assertive::assert_all_are_not_na(dbh, severity = "warning")
  if (!is.null(mindbh)) {
    assertive::assert_is_a_number(mindbh, severity = "warning")
    assertive::assert_all_are_positive(mindbh)
  }
}
