#' Count the total number of individuals in a dataset or within groups.
#' 
#' In an ecological context, these functions calculate abundance. To choose the
#' status of the individuals you want to count, use [count_status()]. If you
#' only want to count alive individuals, you may use the shortcut
#' [count_alive()].
#' 
#' @template x
#' @template .status
#' @param ... Optional. Bare name of variables to group by (passed to 
#'   [dplyr::count()].
#'   
#' @seealso [dplyr::count()].
#'   
#' @return If you provide grouping variables, these functions output a grouped
#'   summary. Else, they output a tally.
#' @export
#' 
#' @examples
#' library(bciex)
#' stems <- bci12s5mini
#' trees <- bci12t5mini
#' 
#' # By default, tally of alive individuals
#' count_alive(stems)
#' # Dead
#' count_status(trees, .status = "D")
#' 
#' # Give one or multiple grouping variables to get a grouped summary
#' count_alive(stems, sp)
#' # Dead by species
#' count_status(stems, .status = "D", sp)
#' 
#' # Multiple groups
#' # Alive
#' stems2 <- rbind(bci12s5mini, bci12s6mini)
#' count_alive(stems2, sp, CensusID)
#' # Dead
#' stems2 <- rbind(bci12s5mini, bci12s6mini)
#' count_status(stems2, .status = "D", sp, CensusID)
count_status <- function(x, .status, ...) {
  if (missing(...)) {
    return(tally_status(x = x, .status = .status))
  }
  count_status_by(x = x, .status = .status, ...)
}

#' @rdname count_status
#' @export
count_alive <- function(x, ...) {
  alive <- "A"
  if (missing(...)) {
    return(count_status(x = x, .status = alive))
  }
  count_status_by(x = x, .status = alive, ...)
}



#' Count individuals of a given status by groups.
#'
#' @template x
#' @template .status 
#' @param ... Optional. Bare name of variables to group by (passed to
#'  [dplyr::count()].
#' 
#' @return A count summary by group.
count_status_by <- function(x, .status, ...) {
  # To later add species excluded by status, so that their count is cero
  all_sp <- unique(x["sp"])
  
  identified <- add_id(x)
  filtered <- filter_status(x = identified, .status = .status)
  grouped_summary <- dplyr::count(filtered, ...)
  # dplyr::count(filtered, sp)
  
  # to excluded species give a count of cero
  all_sp_n <- dplyr::right_join(grouped_summary, all_sp)
  tidyr::replace_na(all_sp_n, list(n = 0))
}



#' Tally individuals of a given status.
#'
#' @template x 
#' @template .status 
#'
#' @return An integer giving the total number (tally) of individuals.
tally_status <- function(x, .status) {
  identified <- add_id(x)
  filtered <- filter_status(x = identified, .status = .status)
  dplyr::tally(filtered)[["n"]]
}



#' Add unique identifier combining treeID and stemID.
#'
#' @template x 
#'
#' @return Returns `x` with an additional variable `id`.
add_id <- function(x) {
  x$id <- paste0(x$treeID, "_", x$stemID)
  x
}

#' Filter individuals of a given status.
#'
#' @template x 
#' @template .status 
#'
#' @return Returns `x` filtered by `.status`.
filter_status <- function(x, .status) {
  stopifnot(all(.status %in% unique(x$status)))
  x[x$status %in% .status, ]
}
