# Warn if all dbh vealues of a census `cns` are equal to NA.
check_if_all_dbh_is_na <- function(cns1, cns2, split) {
  # Check for one census, then appply to both
  check_one_cns <- function(cns, split) {
    # Check if all dbh are NA
    # If no splitting variable is provided do nothing
    if (is.null(split)) {
      return(invisible(NULL))
    }
    cns$grouping <- split
    grouped <- dplyr::group_by(tibble::as.tibble(cns), .data$grouping)
    summarized <- dplyr::summarize(grouped, all_na = all(is.na(.data$dbh)))
    filtered <- dplyr::filter(summarized, .data$all_na)
    nrow(filtered)
  }
  na_sp_n <- unlist(lapply(list(cns1, cns2), check_one_cns, split = split))
  
  if (any(na_sp_n > 0)) {
    warning("At least one split-group contains all `dbh` values equal to NA.\n", 
      "  * Consider removing those groups before running this function.")
  }
}

# Check data has crucial variables
#' @export
#' @keywords internal
check_crucial_names <- function(cns, nms) {
  # Check that names in census1 and census2 are as expected
  are_names_expected <- all(nms %in% names(cns))
  if (are_names_expected) {
    return(invisible(NULL))
  } else {
    stop("Check your data contains these names: ", 
      paste(nms, collapse = ", "))
  }
}
