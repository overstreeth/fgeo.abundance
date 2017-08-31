#' Count individuals
#'
#' @template x 
#' @param group_by Sting giving the variables to group by.
#' @param only.alive 
#'
#' @return
#' @export
#'
#' @examples
#' stem <- bci12t7mini
#' (n <- abundance_n(stem))
#' class(n)
#' 
#' abundance_n(stem, group_by = "sp", F)
abundance_n <- function(x, group_by = c("quadrat", "sp"), only.alive = TRUE) {
{
  if(only.alive) {valid.status <- "A"} else {valid.status <- unique(x$status)}
  table(x[x$status %in% valid.status, group_by])
}

#' Abundance with plyr (deprecated), grouping with a characters string.
#'
#' @template x 
#' @param group_by Character string. Don't know if NULL
#' @param alive_only
#'
#' @return
#' @export
#'
#' @examples
abundance_n_plyr <- function(x, group_by = c("sp", "status"), alive_only = TRUE) {
  n <- plyr::ddply(
    .data = x, 
    .variables = group_by, 
    .fun = function(x) {length(unique(x))}
  )
  
  if (!alive_only) {
    return(n)
  }
  n[n$status == "A", ]
}



#' Abundance with dplyr, grouping with bare variable names.
#'
#' @template  x
#' @param ... Bare names of variables to group by.
#' @param only_alive If TRUE, filter only alive individuals.
#'
#' @return Tally if groups = NULL, else, a count by group.
#' @export
#'
#' @examples
#' abundance_n_dplyr(stem)
#' abundance_n_dplyr(stem, sp, only_alive = FALSE)
abundance_n_dplyr <- function(x, ..., only_alive = TRUE) {
  if (!only_alive) {
    x <- x[x$status == "A", ]
  }
  
  group_by <- rlang::quos(...)
  grouped <- dplyr::group_by(x, !!!group_by)
  dplyr::ungroup(dplyr::summarise(grouped, n = n()))
}



#' Abundance with dplyr, grouping with a characters string.
#'
#' @template x 
#' @param groups Character string giving the names of variables to group by.
#' @param only_alive If TRUE, filter only alive individuals.
#'
#' @return Tally if groups = NULL, else, a count by group.
#' @export
#'
#' @examples
#' abundance_n_dplyr_se(stem)
#' abundance_n_dplyr_se(stem, groups = NULL)
abundance_n_dplyr_se <- function(x, 
                                 groups = c("sp", "status"), 
                                 only_alive = TRUE) {
  if (!only_alive) {
    x <- x[x$status == "A",]
  }
  
  if (is.null(groups)) {
    return(dplyr::tally(x))
  }
  
  # Convert from character string to bare names (https://goo.gl/kPqMUk)
  parsed_groupes <- lapply(groups, parse_quosure)
  grouped <- dplyr::group_by(x, UQS(parsed_groupes))
  
  count <- dplyr::summarise(grouped, n = n())
  dplyr::ungroup(count)
}
