#' Tidy eval helpers
#'
#' These functions provide tidy eval-compatible ways to capture
#' symbols (`sym()`, `syms()`, `ensym()`), expressions (`expr()`,
#' `exprs()`, `enexpr()`), and quosures (`quo()`, `quos()`, `enquo()`).
#' To learn more about tidy eval and how to use these tools, read
#' <http://rlang.tidyverse.org/articles/tidy-evaluation.html>
#'
#' @name tidyeval
#' @keywords internal
#' @aliases          quo quos enquo sym syms ensym expr exprs enexpr quo_name enquos
#' @importFrom rlang quo quos enquo sym syms ensym expr exprs enexpr quo_name enquos
#' @export           quo quos enquo sym syms ensym expr exprs enexpr quo_name
#' @importFrom rlang UQ UQS .data := inform warn abort
NULL



#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom dplyr %>%
#' @usage lhs \%>\% rhs
NULL

# dplyr -------------------------------------------------------------------

#' @importFrom dplyr tally
#' @export
dplyr::tally
#' @importFrom dplyr add_tally
#' @export
dplyr::add_tally

#' @importFrom dplyr count
#' @export
dplyr::count
#' @importFrom dplyr add_count
#' @export
dplyr::add_count

#' @importFrom dplyr group_by
#' @export
dplyr::group_by
#' @importFrom dplyr ungroup
#' @export
dplyr::ungroup



# Avoid CMD check warnings
utils::globalVariables(c(".data", "n", "gx", "gy"))
