#' Warn if all dbh vealues of a census `cns` are equal to NA.
#' @noRd
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

#' Check data has crucial variables
#' @noRd
check_crucial_names <- function(cns, nms) {
  # Check that names in census1 and census2 are as expected
  are_names_expected <- all(nms %in% names(cns))
  if (are_names_expected) {
    return(invisible(NULL))
  } else {
    stop("Check that your data contains these names: ", 
      paste(nms, collapse = ", "), call. = FALSE)
  }
}

#' Wrap multiple checks in map_sp() and map_sp_pdf() for clarity.
#' @noRd
check_map_sp <- function(cns, sp) {
  assertive::assert_is_data.frame(cns)
  check_crucial_names(cns = cns, nms = c("gx", "gy", "sp"))
  assertive::assert_is_character(sp)
  assertive::assert_is_non_empty(sp)
}

#' If file extension is not .pdf, ignore the given file-name
#' @noRd
check_file_extension <- function(file) {
  is_extension_ok <- grepl("*.\\.pdf", file)
  if (is_extension_ok) {
    return(file)
  } else {
    warning("File extension should be .pdf.\n",
      "  * Replacing given file name by default file name")
    file <- "map.pdf"
  }
  file
}
