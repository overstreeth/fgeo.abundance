#' @importFrom fgeo.tool by_group flag_if_group
#' @importFrom fgeo.base check_crucial_names insensitive rename_matches
#' @importFrom fgeo.base is_duplicated is_multiple
#' @importFrom dplyr group_by ungroup filter select mutate summarize arrange
#' @importFrom dplyr count
#' @importFrom glue glue glue_collapse
#' @importFrom rlang set_names %||% abort warn inform has_name
NULL

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
#' @export           quo quos enquo sym syms ensym expr exprs enexpr quo_name enquos
#' @importFrom rlang UQ UQS .data := inform warn abort set_names
NULL

#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
NULL

# Avoid CMD check warnings
utils::globalVariables(c(".data", ".", "n", "gx", "gy"))
