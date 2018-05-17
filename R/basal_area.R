#' Basal area.
#'
#' Basal area of each individual (`basal_area_ind()`).
#'
#' Units of returned values are the square of the input units. For example, if
#' you input diameter measured in mm, the output will be an area measured in
#' mm^2.
#'
#' @template x
#' @template group_by
#' @template only_alive
#' @param diameter A vector giving the individuals' diameter (generally, `x$dbh`).
#'
#' @return Each individual calculation is simply the area of a circle given its
#'   diameter; thus the unit of the returned value is the square of the input
#'   units unit (i.e. for input in mm, output is mm^2; for input in cm, output is
#'   cm^2, and so on).
#'   * `basal_area_ind()`: A numeric vector giving the basal area of each
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
#' head(basal_area_ind(stem$dbh))
#'
#' head(basal_area(stem))
#'
#' # Silent
#' head(suppressMessages(basal_area(stem)))
basal_area <- function(x, group_by = c("quadrat", "sp"), only_alive = TRUE) {
  grouped <- group(x = x, group_by = group_by, only_alive = only_alive)

  dplyr::summarise(
    grouped, 
    basal_area = sum(basal_area_ind(.data$dbh), na.rm = TRUE)
  )
}

#' @export
#' @rdname basal_area
basal_area_ind <- function(diameter) {
  check_basal_area_ind(d = diameter)
  1 / 4 * pi * (diameter)^2
}

check_basal_area_ind <- function(d) {
  
  if (identical(length(d), 0)) {
    abort(paste(
      "The vector passed to `diameter` is empty.\n",
      "  * Provide a non empty vector."
    ))
  }
  invisible(d)
}  
