#' Name of species which count is at or above a threshold.
#'
#' Name of species which count is at or above a threshold.
#'
#' @template censdata
#' @template abundances
#' @template n
#'
#' @name sp_names_by_n
#'
#' @return A character string giving the names of species which count of alive
#'   stems ([sp_alive_n()]) or total abundance per quadrat ([sp_abund_n()]) is
#'   at or above a threshold.
#'
#' @examples
#' # With census data
#' sp_alive_n(censdata = bci_mini, n = 1500)
#'
#' # With abundance data
#' abund <- abundance_match_census_habitat(bci_mini, bci_habitat)
#' sp_abund_n(abund, n = 1500)
NULL

#' @rdname sp_names_by_n
#' @export
sp_alive_n <- function(censdata, n) {
  validate_sp_alive_n(censdata, n)

  alive_stems <- censdata[censdata$status == "A", "sp"]
  alive_stems_count <- table(alive_stems)

  stop_if_n_is_too_high(x = alive_stems_count, n = n)

  alive_n_plus <- alive_stems_count[alive_stems_count >= n]
  names(alive_n_plus)
}

#' @rdname sp_names_by_n
#' @export
sp_abund_n <- function(abundances, n) {
  validate_sp_abund_n(abundances, n)

  rs <- rowSums(abundances)
  stop_if_n_is_too_high(x = rs, n = n)

  names(rs[rs >= n])
}



#' Assert important names match names in data.
#'
#' @param .data A matrix or dataframe.
#' @param match  Names to match with names of `.data`.
#' @param ... Arguments passed to [assertive::assert_all_are_matching_regex()],
#'   e.g. `severity = "stop"` (default), `severity = "warning"`.
#'
#' @return Nothing, warning, error, etc. See 
#'   [assertive::assert_all_are_matching_regex()].
#'
#' @examples
#' # Passes silently
#' assert_are_names_matching(data.frame(a = 1), match = c("a"))
#' # Errs
#' testthat::expect_error(
#'   assert_are_names_matching(data.frame(a = 1), match = c("aa"))
#' )
#' # Warns
#' assert_are_names_matching(data.frame(a = 1), match = c("aa"),
#'   severity = "warning")
#' @keywords internal
#' @export 
assert_are_names_matching <- function(.data, match, ...) {
  stopifnot(is.data.frame(.data) || is.matrix(.data))
  assertive::assert_is_character(match)

  anchored_names <- paste0("^", colnames(.data), "$")
  collapsed_names <- paste(anchored_names, collapse = "|")
  assertive::assert_all_are_matching_regex(match, collapsed_names, ...)
}

# Name of species which stems are n or above 

# Provide readable conditional stop, with friendly message
stop_if_n_is_too_high <- function(x, n) {
  assertive::assert_is_numeric(x)
  assertive::assert_is_numeric(n)
  assertive::assert_is_scalar(n)

  if (n > max(x)) {
    stop(
      "No species has so many alive stems as n = ", deparse(n), ". ",
      "Provide n <= ", deparse(max(x)), "."
    )
  }
}



# validate ----------------------------------------------------------------

validate_sp_alive_n <- function(censdata, n) {
  assertive::assert_is_data.frame(censdata)
  assertive::assert_is_scalar(n)
  assertive::assert_all_are_non_negative(n)
}

validate_sp_abund_n <- function(abundances, n) {
  assertive::assert_is_data.frame(abundances)
  assertive::assert_is_non_empty(n)
  assertive::assert_is_scalar(n)
  assertive::assert_is_numeric(n)
}
