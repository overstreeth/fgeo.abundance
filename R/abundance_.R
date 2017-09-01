# Using base (by Gabriel) -------------------------------------------------

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
abundance_n <- function(x, group_by = c("quadrat", "sp"), only.alive = TRUE)
{
  if(only.alive) {valid.status <- "A"} else {valid.status <- unique(x$status)}
  table(x[x$status %in% valid.status, group_by])
}



# Using dplyr and plyr (by Mauro) -----------------------------------------

#' Abundance with dplyr and plyr (deprecated)
#' 
#' * Abundance with dplyr, grouping with bare variable names.
#'   * [abundance_n_dplyr()] uses the latest "tidy eval" approach.
#'   
#' * Abundance with dplyr, grouping with a character string
#'   * [abundance_n_dplyr_se()] uses latest tidy eval approach.
#'   * [abundance_n_dplyr_se_()] uses deprecated approach.
#'   * [abundance_n_plyr()] uses plyr (deprecated).
#'
#' @template  x
#' @param ... Bare names of variables to group by.
#' @param groups Character string giving the names of variables to group by.
#' @param only_alive If TRUE, filter only alive individuals.
#'
#' @return Tally if groups = NULL, else, a count by group.
#' @export
#' @name abundance_n_d_plyr
#'
#' @examples
#' stem <- bci12s7mini
#' 
#' # Using plyr, the deprecated ancestor of dplyr.
#' head(abundance_n_plyr(stem))
#' head(abundance_n_plyr(stem), groups = c("quadrat", "sp"), alive_only = FALSE)
#' 
#' # Using non-standard evaluation.
#' abundance_n_dplyr(stem)
#' abundance_n_dplyr(stem, sp, only_alive = FALSE)
#' 
#' # Standard evaluation using the tidy eval approach.
#' abundance_n_dplyr_se(stem)
#' abundance_n_dplyr_se(stem, groups = NULL)
#' 
#' # Standard evaluation using older approach. See ?group_by_ (ends in "_").
#' abundance_n_dplyr_se_(stem)
#' abundance_n_dplyr_se_(stem, groups = c("status"))
#' abundance_n_dplyr_se_(stem, groups = NULL)
#' 

#' @rdname abundance_n_d_plyr
#' @export
abundance_n_dplyr <- function(x, ..., only_alive = TRUE) {
  if (!only_alive) {
    x <- x[x$status == "A", ]
  }
  
  group_by <- rlang::quos(...)
  grouped <- dplyr::group_by(x, !!!group_by)
  dplyr::ungroup(dplyr::summarise(grouped, n = n()))
}

#' @rdname abundance_n_d_plyr
#' @export
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

#' @rdname abundance_n_d_plyr
#' @export
abundance_n_dplyr_se_ <- function(x, 
                                  groups = c("sp", "status"), 
                                  only_alive = TRUE) {
  if (!only_alive) {
    x <- x[x$status == "A",]
  }
  
  if (is.null(groups)) {
    return(dplyr::tally(x))
  }

  grouped <- dplyr::group_by_(x, groups)
  count <- dplyr::summarise(grouped, n = n())
  dplyr::ungroup(count)
}

#' @rdname abundance_n_d_plyr
#' @export
abundance_n_plyr <- function(x, 
                             groups = c("sp", "status"), 
                             alive_only = TRUE) {
  n <- plyr::ddply(
    .data = x, 
    .variables = groups, 
    .fun = function(x) {length(unique(x))}
  )
  
  if (!alive_only) {
    return(n)
  }
  n[n$status == "A", ]
}
