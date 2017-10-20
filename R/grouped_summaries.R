#' A version of [dplyr::group_by()] that inputs "quoted" variable names.
#' 
#' @template x
#' @template group_by 
#' @template only_alive
#' 
#' @seealso [dplyr::group_by()].
#' 
#' @return A grouped tibble.
#' @export
#' @examples 
#' df <- bciex::bci12s7mini
#' group(df, "sp", only_alive = TRUE)
#' group(df, c("quadrat", "sp"), only_alive = TRUE)
group <- function(x, group_by, only_alive) {
  assertive::assert_is_non_empty(x)
    
  if (only_alive) {
    x <- x[x$status == "A", ]
  }
  
  if (is.null(group_by)) {
    stop("group_by can't be NULL.")
  }
  
  # Convert from character string to bare names (https://goo.gl/kPqMUk)
  parsed_groups <- lapply(group_by, parse_quosure)
  dplyr::group_by(x, UQS(parsed_groups))
}



# Abundance ---------------------------------------------------------------

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
#' # Instead of a data frame, return an integer representing a tally
#' abundance_tally(stem)
#' 
#' # Count not only alive
#' n <- abundance(stem, group_by = c("status", "sp"), only_alive = FALSE)
#' ordered_by_species <- order(n$sp)
#' head(n[ordered_by_species, ])
#' 
#' 
#' 
#' # Alternatives ----------------------------------------------------------
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
  grouped <- group(x = x, group_by = group_by, only_alive = only_alive)
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



# Basal area --------------------------------------------------------------

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
#' stem <- bci12s7mini
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
  assertive::assert_is_non_empty(diameter)
  
  message("Units of returned values are the square of the input units.")
  1/4 * pi * (diameter)^2
}
