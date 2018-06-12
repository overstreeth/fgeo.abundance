#' Drop if missing values.
#' 
#' Valuable mostly for its warning.
#'
#' @param dfm A dataframe.
#' @param x String giving a column name of `dfm`.
#'
#' @return A dataframe.
#' @export
#'
#' @examples
#' dfm <- data.frame(a = 1, b = NA)
#' drop_if_na(dfm, "b")
#' drop_if_na(dfm, "a")
drop_if_na <- function(dfm, x) {
  .x <- dfm[[x]]
  missing <- is.na(.x)
  if (any(missing)) {
    warning(
      "Dropping ", sum(missing), " rows with missing `", x, "` values.", 
      call. = FALSE
    )
  }
  dfm[!missing, , drop = FALSE]
}
