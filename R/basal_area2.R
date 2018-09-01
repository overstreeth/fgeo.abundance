ba_df <- function(x) {
  g <- dplyr::group_vars(x)
  out <- summarize(x, basal_area = sum(ba_dbl(dbh), na.rm = TRUE))
  dplyr::grouped_df(out, g)
}

ba_dbl <- function(x) {
  stopifnot(length(x) > 0)
  1 / 4 * pi * (x)^2
}
