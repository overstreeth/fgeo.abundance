# Warn if all dbh vealues of a census `cns` are equal to NA.
check_if_all_dbh_is_na <- function(cns, split) {
  # If no splitting variable is provided do nothing
  if (is.null(split)) {
    return(invisible(NULL))
  }
  
  cns$grouping <- split
  grouped <- dplyr::group_by(tibble::as.tibble(cns), .data$grouping)
  summarized <- dplyr::summarize(grouped, all_na = all(is.na(.data$dbh)))
  filtered <- dplyr::filter(summarized, .data$all_na)
  na_sp_n <- nrow(filtered)
  if (na_sp_n > 0) {
    warning(na_sp_n, " split-groups contain all `dbh` values equal to NA.\n", 
      "  * Consider removing those groups before running this function.")
  }
}
