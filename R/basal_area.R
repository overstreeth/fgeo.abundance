#' Basal area of each individual.
#' 
#' `basal_area()` is a generic function with methods for numeric vectors and 
#' dataframes. To calculate basal area within groups, feed
#' `basal_area()` with a dataframe previously grouped with `group_by()` (see
#'  examples and section See also). `group_by()` comes from __dplyr__ but for
#'  convenience it is reexported by __fgeo.abundance__.
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
#' stem <- fgeo.data::luquillo_stem_random_tiny
#' 
#' # Calculate basal area on a vector
#' ba <- basal_area(stem$dbh)
#' ba
#' 
#' # Add result to the original dataframe
#' stem$ba <- ba
#' # Same
#' stem <- mutate(stem, ba = basal_area(dbh))
#' # Reordering columns to show `ba` first
#' select(stem, ba, everything())
#' 
#' # Calculate basal area on the entire dataframe
#' basal_area(stem)
#' 
#' # Now by groups
#' basal_area(group_by(stem, quadrat))
#' 
#' grouped <- group_by(stem, quadrat, sp)
#' basal_area(grouped)
#' 
#' # Also works on ViewFullTable
#' vft <- fgeo.data::luquillo_vft_4quad
#' head(basal_area(vft$DBH))
#' 
#' grouped_vft <- group_by(vft, CensusID, QuadratName)
#' basal_area(grouped_vft, dbh = DBH)
basal_area <- function(x, ...) {
  UseMethod("basal_area")
}

#' @export
#' @rdname basal_area
basal_area.default <- function(x, ...) {
  abort(paste("Can't deal with objects of class", class(x)))
}

#' @export
#' @rdname basal_area
basal_area.data.frame <- function(x, dbh = dbh, ...) {
  dbh <- enquo(dbh)
  dplyr::summarise(x, basal_area = sum(basal_area.numeric(!!dbh), na.rm = TRUE))
}

#' @export
#' @rdname basal_area
basal_area.numeric <- function(x, ...) {
  stopifnot(length(x) > 0)
  1 / 4 * pi * (x)^2
}
