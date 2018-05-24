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
#' @aliases          quo quos enquo sym syms ensym expr exprs enexpr quo_name
#' @importFrom rlang quo quos enquo sym syms ensym expr exprs enexpr quo_name
#' @export           quo quos enquo sym syms ensym expr exprs enexpr quo_name
#' @importFrom rlang UQ UQS .data :=
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

#' Reexports from dplyr.
#' 
#' @name reexports
#' @keywords internal
#' @aliases          tally count add_tally add_count group_by
#' @importFrom dplyr tally count add_tally add_count group_by
#' @export           tally count add_tally add_count group_by
NULL

