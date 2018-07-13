#' Basal area of each individual.
#'
#' * `basal_area()` is a generic function with methods for numeric vectors and
#' dataframes. To calculate basal area within groups, feed `basal_area()` with a
#' dataframe previously grouped with [dplyr::group_by()].
#' * `add_basal_area()`
#' adds a column `basal_area`.
#'
#' @param x Numeric vector or dataframe giving the individuals' diameter.
#' @param dbh If `x` is a dataframe, `dbh` is the bare name of the column giving
#'   individual's diameter.
#' @param ... Other arguments passed to methods.
#'
#' @seealso [dplyr::group_by()].
#'
#' @return Each individual calculation is simply the area of a circle given its
#'   diameter; thus the unit of the returned value is the square of the input
#'   units unit (i.e. for input in mm, output is mm^2; for input in cm, output is
#'   cm^2, and so on).
#'   * `basal_area.numeric()`: A numeric vector giving the basal area of each
#'   individual.
#'
#' @family functions that quote one or more arguments using tidy eval.
#'
#' @export
#' @rdname basal_area
#'
#' @examples
#' library(dplyr)
#'
#' stem <- tibble(
#'   stemID = 1:6,
#'   quadrat = paste0("000", rep(1:2, each = 3)),
#'   sp = c(paste0("sp", c(1, 1, 2)), paste0("sp", c(3, 3, 3))),
#'   dbh = abs(sample(rnorm(100), 6) * 10)
#' )
#' stem
#' basal_area(stem$dbh)
#' add_basal_area(stem)
#'
#' # Sum across all rows
#' basal_area(stem)
#'
#' # Sum across rows by groups
#' basal_area(
#'   group_by(stem, quadrat, sp)
#' )
#' # Same but returns all rows
#' add_basal_area(stem, quadrat, sp)
#'
#' # Dealing with missing values
#' stem2 <- stem
#' stem2[6, 4] <- NA
#' stem2
#'
#' # basal_area() of missing dbh in a vector results in NA
#' basal_area(stem2$dbh)
#' # add_basal_area() of missing dbh in a dataframe results in NA
#' # Same if using add_basal_area()
#' add_basal_area(stem2)
#'
#' # But basal_area() of missing dbh in a dataframe ignores NA's
#' basal_area(stem2)
#' basal_area(group_by(stem2, quadrat, sp))
#' # Missing dbh is ignored, and `basal_area` refers to the group not each row.
#' add_basal_area(stem2, quadrat, sp)
basal_area <- function(x, ...) {
  UseMethod("basal_area")
}

#' @rdname basal_area
#' @export
basal_area.default <- function(x, ...) {
  abort(paste("Can't deal with objects of class", class(x)))
}

#' @rdname basal_area
#' @export
basal_area.data.frame <- function(x, dbh = dbh, ...) {
  dbh <- enquo(dbh)
  summarize(x, basal_area = sum(basal_area.numeric(!!dbh), na.rm = TRUE))
}

#' @rdname basal_area
#' @export
basal_area.numeric <- function(x, ...) {
  stopifnot(length(x) > 0)
  1 / 4 * pi * (x)^2
}

#' @rdname basal_area
#' @export
add_basal_area <- function(x, ..., dbh = dbh) {
  dbh <- enquo(dbh)
  g <- dplyr::group_vars(x)
  grouped <- group_by(x, ..., add = TRUE)
  out <- add_basal_area_tally(grouped, !!dbh)
  dplyr::grouped_df(out, g)
}

add_basal_area_tally <- function(x, dbh = dbh) {
  dbh <- enquo(dbh)

  check_basal_area_tally(x)
  mutate(x, basal_area = sum(basal_area.numeric(!!dbh), na.rm = TRUE))
}

check_basal_area_tally <- function(x) {
  stopifnot(is.data.frame(x))
  preexisting_col <- any(grepl("basal_area", names(x)))
  if (preexisting_col) {
    warn("Overwritting preexisting column `basal_area`.")
  }
  invisible(x)
}
