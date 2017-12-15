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
#' @return
#'   * `basal_area_ind()`: A numeric vector giving the basal area of each
#'     individual in the same units as the input vector. This is simply the
#'     area of a circle given its diameter.
#' @family grouped summaries
#' @export
#' @rdname basal_area
#'
#' @examples
#' stem <- bciex::bci12s7mini
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
  
  sum_basal_area <- function(diameter) {
    sum(suppressMessages(basal_area_ind(diameter)), na.rm = TRUE)
  }
  count <- dplyr::summarise(grouped, basal_area = sum_basal_area(.data$dbh))
  message("Units of returned values are the square of the input units.")
  as.data.frame(count, stringsAsFactors = FALSE)
}

#' @export
#' @rdname basal_area
basal_area_ind <- function(diameter) {
  if (length(diameter) == 0) {
    stop(
      "The vector passed to `diameter` is empty.\n",
      "  * Provide a non empty vector."
    )
  }
  
  message("Units of returned values are the square of the input units.")
  1/4 * pi * (diameter)^2
}
