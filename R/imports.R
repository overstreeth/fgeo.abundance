#' @importFrom fgeo.base check_crucial_names insensitive rename_matches
#' @importFrom fgeo.base is_duplicated is_multiple
#' @importFrom dplyr group_by ungroup filter select mutate summarize arrange
#' @importFrom dplyr count
#' @importFrom glue glue
#' @importFrom rlang set_names %||% abort warn inform
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
utils::globalVariables(c(".data", "n"))
