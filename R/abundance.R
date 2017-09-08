# Using base and beyond ---------------------------------------------------

#' Count number of individuals in total or by groups.
#' 
#' Count number of individuals in total (`abundance_tally()`) or by groups 
#' (`abundance()`).
#'
#' @param  x Census data.
#' @param group_by Character string giving the names of variables to group by.
#' @param only_alive If TRUE, counts only alive individuals (`status == "A"`).
#'   If FALSE counts individuals of all available statuses.
#'
#' @return
#'   * `abundance_tally()`: An integer giving the number of rows in `x`, which 
#'     for census data equals the number of unique individuals.
#'   * `abundance()`: a data frame giving the number of individuals in each 
#'     group defined by `group_by`.
#'     
#' @export
#' @author Gabriel Arellano, Sean McMahon & Mauro Lepore.
#' @seealso [dplyr::tally()], [dplyr::count].
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
  as.data.frame(count)
}

#' @rdname abundance
#' @export
abundance_tally <- function(x, only_alive = TRUE) {
  if (only_alive) {
    x <- x[x$status == "A", ]
  }
  dim(x)[1L]
}
