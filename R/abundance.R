#' Count number of individuals in total or by groups.
#' 
#' Count number of individuals in total (`abundance_tally()`) or by groups 
#' (`abundance()`).
#'
#' @template  x
#' @template group_by
#' @template only_alive
#'
#' @return
#'   * `abundance_tally()`: An integer giving the number of rows in `x`, which 
#'     for census data equals the number of unique individuals.
#'   * `abundance()`: a data frame giving the number of individuals in each 
#'     group defined by `group_by`.
#'     
#' @export
#' @author Gabriel Arellano, Sean McMahon & Mauro Lepore.
#' @seealso [dplyr::tally()], [dplyr::count()].
#' @family grouped summaries
#' @examples
#' stem <- bci12s7mini
#' 
#' # Returns a data frame by groups
#' n <- abundance(stem)
#' head(n)
#' 
#' # Safest functions always return the same type of output. This errs:
#' \dontrun{
#' abundance(stem, group_by = NULL)
#' }
#' 
#' # Returns not a data frame but an integer representing a tally
#' n <- abundance_tally(stem)
#' head(n)
#' 
#' # Count not only alive
#' n <- abundance(stem, group_by = c("status", "sp"), only_alive = FALSE)
#' head(n)
#' 
#' 
#' 
#' # Same, using dplyr directly
#' 
#' library(dplyr)
#' 
#' alive <- filter(stem, status == "A")
#' 
#' grouped <- group_by(alive, quadrat, sp)
#' summarise(grouped, n = n())
#' 
#' # Or
#' count(alive, quadrat, sp)
#' @export
abundance <- function(x, group_by = c("quadrat", "sp"), only_alive = TRUE) {
  assertive::assert_is_non_empty(x)
    
  if (only_alive) {
    x <- x[x$status == "A", ]
  }
  
  if (is.null(group_by)) {
    stop("group_by can't be NULL. If you want a tally, see ?abundance_tally.")
  }
  
  # Convert from character string to bare names (https://goo.gl/kPqMUk)
  parsed_groups <- lapply(group_by, rlang::parse_quosure)
  grouped <- dplyr::group_by(x, rlang::UQS(parsed_groups))
  
  count <- dplyr::summarise(grouped, n = n())
  as.data.frame(count, stringsAsFactors = FALSE)
}

#' @rdname abundance
#' @export
abundance_tally <- function(x, only_alive = TRUE) {
  if (only_alive) {
    x <- x[x$status == "A", ]
  }
  dim(x)[1L]
}




















# DRY: group before summarizing in family abundance(), basal_area(), etc ----

group <- function(x, group_by, only_alive) {
  assertive::assert_is_non_empty(x)
    
  if (only_alive) {
    x <- x[x$status == "A", ]
  }
  
  if (is.null(group_by)) {
    stop("group_by can't be NULL.")
  }
  
  # Convert from character string to bare names (https://goo.gl/kPqMUk)
  parsed_groups <- lapply(group_by, rlang::parse_quosure)
  dplyr::group_by(x, rlang::UQS(parsed_groups))
}



# Basal area ----

#' Basal area.
#' 
#' Basal area of each individual (`basal_area_ind()`).
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
#' stem <- bci12s7mini
#' 
#' head(basal_area_ind(stem$dbh))
#' head(basal_area(stem))
#' head(suppressMessages(basal_area(stem)))
basal_area <- function(x, group_by = c("quadrat", "sp"), only_alive = TRUE) {
  grouped <- group(x = x, group_by = group_by, only_alive = only_alive)
  
  sum_basal_area <- function(diameter) {
    sum(suppressMessages(basal_area_ind(diameter)), na.rm = TRUE)
  }
  count <- dplyr::summarise(grouped, basal_area = sum_basal_area(.data$dbh))
  message("Returning basal area in the same units as input.")
  as.data.frame(count, stringsAsFactors = FALSE)
}

#' @export
#' @rdname basal_area
basal_area_ind <- function(diameter) {
  assertive::assert_is_non_empty(diameter)
  
  message("Returning basal area in the same units as input.")
  1/4 * pi * (diameter)^2
}
