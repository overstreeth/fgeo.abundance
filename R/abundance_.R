# Using base --------------------------------------------------------------

#' Count individuals.
#'
#' @template x 
#' @template group_by
#' @template only_alive
#' 
#' @author Gabriel Arellano.
#'
#' @return As per [base::table()].
#' @seealso [base::table()].
#' @export
#'
#' @examples
#' stem <- forestr::bci12t7mini
#' (n <- abundance_n(stem))
#' class(n)
#' 
#' abundance_n(stem, group_by = "sp", FALSE)
abundance_n <- function(x, group_by = c("quadrat", "sp"), only_alive = TRUE) {
    if (only_alive) {
      valid.status <- "A"
    } else {
      valid.status <- unique(x$status)
    }
    table(x[x$status %in% valid.status, group_by])
}




#' Count individuals.
#'
#' @template x
#' @template only_alive 
#' @template group_by
#' @param ... Arguments passed to [base::tapply()].
#' 
#' @seealso [base::tapply()].
#'
#' @return Count of individuals in each group.
#' @export
#'
#' @examples
#' stem <- forestr::bci12s6mini
#' head(abundance_n2(stem))
#' head(abundance_n2(stem, group_by = c("status"), only_alive = FALSE))
#' head(abundance_n2(stem, group_by = c("status"), only_alive = FALSE))
abundance_n2 <- function(x, 
              group_by = c("sp", "status"), 
              only_alive = TRUE, 
              ...) {
  if (only_alive) {x <- x[x$status == "A", ]}
  
  idx <- x[group_by]
  .x <- x[[group_by[1]]]
  
  tapply(X = .x, INDEX = idx, FUN = function(x){length(x)}, ...)
}



# Using base and beyond ---------------------------------------------------

#' These functions are alternatives which source code use __dplyr__ or __plyr__.
#' 
#' Compared to using __base__ exclusively, these functions may be a little more
#' friendly for users, but maybe harder for developers to understand and 
#' maintain. __dplyr__ evolves very rapidly and therefore some functions that
#' are recommended today may be deprecated later. Yet, the source code is
#' available so it is always possible to re-use it into __forestr__.
#' 
#' * Grouping with a character string
#'   * [n_dplyr_se()] uses latest tidy eval approach.
#'   * [n_dplyr_se_()] uses deprecated approach.
#'   * [n_plyr()] uses plyr (deprecated).
#'
#' * Grouping with bare variable names:
#'   * [n_dplyr()] uses the latest "tidy eval" approach.
#'
#' @template  x
#' @param ... Bare names of variables to group by.
#' @param groups Character string giving the names of variables to group by.
#' @param only_alive If TRUE, filter only alive individuals.
#'
#' @return Tally if groups = NULL, else, a count by group.
#' @export
#' @name n_d_plyr
#'
#' @examples
#' library(dplyr)
#' 
#' stem <- bci12s7mini
#' 
#' # Giving grouping variables in a character string  -----------------------
#' 
#' # USING DPLYR (IF NEW TO DPLYR, THE SOURCE CODE MAY BE HARDER TO UNDERSTAND)
#' 
#' # Standard evaluation using the newest (tidy eval) approach.
#' 
#' n_dplyr_se(stem)
#' 
#' # Same:
#' n_dplyr_se(stem, groups = c("status", "sp"))
#' 
#' # NULL groups result in a tally.
#' n_dplyr_se(stem, groups = NULL)
#' 
#' # Count not only alive
#' n <- n_dplyr_se(stem, groups = c("status", "sp"), only_alive = FALSE)
#' arrange(n, sp)
#' 
#' 
#' # Standard evaluation using an older approach. See ?group_by_ (ends in "_").
#' 
#' n_dplyr_se_(stem)
#' 
#' n_dplyr_se_(stem, groups = NULL)
#' 
#' n_dplyr_se_(stem, groups = c("status"), only_alive = FALSE)
#' 
#' 
#' 
#' # INSTEAD OF DPLYR, USING PLYR (DEPRECATED, AND SLOWER THAN BASE AND DPLYR)
#' 
#' # Using plyr, the deprecated ancestor of dplyr
#' 
#' head(n_plyr(stem))
#' 
#' head(n_plyr(stem), groups = c("quadrat", "sp"), only_alive = FALSE)
#' 
#' df <- data.frame(
#'   id = 1:6,
#'   sp = letters[c(1, 2, 2, 3, 3, 3)],
#'   status = rep(c("A", "D"), each = 3),
#'   stringsAsFactors = FALSE
#' )
#' df
#' 
#' # The same ----
#' 
#' dplyr::count(df, sp, status)
#' 
#' grouped <- dplyr::group_by(df, sp, status) 
#' dplyr::summarise(grouped, n = n())
#' 
#' tibble::as_tibble(
#'   n_plyr(df, c("sp", "status"), only_alive = FALSE)
#' )
#' 
#' # Giving grouping variables as bare names --------------------------------
#' 
#' # dplyr defaults to using bare variable names. 
#' 
#' n_dplyr(stem)
#' 
#' n_dplyr(stem, sp, only_alive = FALSE)
#' 
#' @rdname n_d_plyr
#' @export
n_dplyr <- function(x, ..., only_alive = TRUE) {
  if (only_alive) {
    x <- x[x$status == "A", ]
  }
  
  group_by <- rlang::quos(...)
  grouped <- dplyr::group_by(x, !!!group_by)
  dplyr::ungroup(dplyr::summarise(grouped, n = n()))
}

#' @rdname n_d_plyr
#' @export
n_dplyr_se <- function(x, 
                       groups = c("status", "sp"), 
                       only_alive = TRUE) {
  if (only_alive) {
    x <- x[x$status == "A", ]
  }
  
  if (is.null(groups)) {
    return(dplyr::tally(x))
  }
  
  # Convert from character string to bare names (https://goo.gl/kPqMUk)
  parsed_groupes <- lapply(groups, rlang::parse_quosure)
  grouped <- dplyr::group_by(x, rlang::UQS(parsed_groupes))
  
  count <- dplyr::summarise(grouped, n = n())
  dplyr::ungroup(count)
}

#' @rdname n_d_plyr
#' @export
n_dplyr_se_ <- function(x, 
                        groups = c("sp", "status"), 
                        only_alive = TRUE) {
  if (only_alive) {
    x <- x[x$status == "A",]
  }
  
  if (is.null(groups)) {
    return(dplyr::tally(x))
  }

  grouped <- dplyr::group_by_(x, groups)
  count <- dplyr::summarise(grouped, n = n())
  dplyr::ungroup(count)
}

#' @rdname n_d_plyr
#' @export
n_plyr <- function(x, 
                   groups = c("sp", "status"), 
                   only_alive = TRUE,
                   ...) {
  # temporarily identify each individual uniquely
  x <- add_id(x)
  
  n <- plyr::ddply(
    .data = x, 
    .variables = groups, 
    .fun = function(...) dplyr::summarise(n = n(), ...)
  )
  # remove temporary variable to leave data "as is"
  n$id <- NULL
  
  if (!only_alive) {
    return(n)
  }
  n[n$status == "A", ]
}




# Helper ------------------------------------------------------------------

# not sure yet if we need this.

#' Add unique identifier combining treeID and stemID.
#'
#' @template x 
#' 
#' @return Returns `x` with an additional variable `id`.
#' @keywords internal
add_id <- function(x) {
  # match names regardless of case
  old_names <- names(x)
  names(x) <- tolower(names(x))
  
  x$id <- paste0(x$treeid, "_", x$stemid)
  # restore old names
  names(x) <- old_names
  x
}
