#' Fill 2 dimensional data with custom value.
#'
#' This function fills out an array of 2 dimensions with custom values for extra
#' columns and rows as named in `class1` and `class2`. If a column or row is
#' missing, it will be filled with the value given by fill. It is useful for
#' results of table or tapply when some elements had no records.
#' 
#' @param .data A two-dimension array, matrix or data.frame.
#' @param class1,class2 A vector of the same length  of `.data` or longer.
#' @param fill A numeric value to fill missing values with.
#'
#' @aliases fill.dimension
#' 
#' @export
#' @examples 
#' class1 <- 1:2
#' class2 <- letters[1:5]
#' fill = 0
#' .data <- array(
#'   data = c(1, 2, NA),
#'   dim = c(2, 4),
#'   dimnames = list(1:2, letters[1:4])
#' )
fill_dimension <- function(.data, class1, class2, fill = 0) {
  validate_fill_dimension(.data, class1, class2, fill)
  
  result <- data.frame(
    matrix(fill, nrow = length(class1), ncol = length(class2))
  )
  rownames(result) <- class1
  colnames(result) <- class2

  result[rownames(.data), colnames(.data)] <- .data
  result[is.na(result)] <- fill
  result
}



# validate ----------------------------------------------------------------

validate_fill_dimension <- function(.data, class1, class2, fill) {
  # let class1 = NULL pass this test
  if (!is.null(class1)) {
    assertive::assert_any_are_not_na(class1, severity = "warning")
  }
  assertive::assert_is_of_length(dim(.data), 2, severity = "warning")
  assertive::assert_any_are_matching_regex(
    class(.data), "matrix|data.frame", severity = "warning"
  )
  class2_is_not_shorter_than_n_cols_of_data <- length(class2) >= ncol(.data)
  assertive::assert_is_identical_to_true(
    class2_is_not_shorter_than_n_cols_of_data, severity = "stop"
  )
}
