#' Drop if missing values.
#' 
#' Valuable mostly for its warning.
#'
#' @param dfm A dataframe.
#' @param x String giving a column name of `dfm`.
#'
#' @return
#' @export
#'
#' @examples
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
