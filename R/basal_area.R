#' Basal area of each individual.
#' 
#' Basal area of each individual. The input can be either a vector or a
#' dataframe.
#'
#' @param x A vector giving the individuals' diameter or a dataframe with 
#'   individuals' diameter stored in the column `dbh`.
#' @param group_by Character string giving the names of variables to group by.
#' @param only_alive If TRUE, counts only alive individuals (`status == "A"`).
#'   If FALSE counts individuals of all available statuses.
#'
#' @return Each individual calculation is simply the area of a circle given its
#'   diameter; thus the unit of the returned value is the square of the input
#'   units unit (i.e. for input in mm, output is mm^2; for input in cm, output is
#'   cm^2, and so on).
#'   * `basal_area.numeric()`: A numeric vector giving the basal area of each
#'   individual.
#' 
#' @family grouped summaries
#' 
#' @export
#' @rdname basal_area
#'
#' @examples
#' stem <- fgeo.data::luquillo_stem_random_tiny
#'
#' head(stem$dbh)
#' head(basal_area.numeric(stem$dbh))
#'
#' head(basal_area(stem))
#'
#' # Silent
#' head(suppressMessages(basal_area(stem)))
basal_area <- function(x, group_by, only_alive) {
  UseMethod("basal_area")
}

#' @export
basal_area.default <- function(x) {
  abort(paste("Can't deal with objects of class", class(x)))
}

#' @export
basal_area.data.frame <- function(x) {
  dplyr::summarise(
    x, basal_area = sum(basal_area.numeric(.data$dbh), na.rm = TRUE)
  )
}

#' @export
basal_area.numeric <- function(x) {
  stopifnot(length(x) > 0)
  1 / 4 * pi * (x)^2
}
