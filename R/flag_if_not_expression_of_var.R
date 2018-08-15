#' Inform, warn or abort if not all expresisons refer to a given variable.
#'
#' @param dots Expressions, usually passed to dplyr::filter() via `...`.
#' @param .flag Rlang flag funcitons: inform, warn, and abort.
#' @param .var String of lenght one giving the name of the variable expected to
#'   be referred in the expressions passed to `...`.
#' @noRd
flag_if_not_expression_of_var <- function(dots, .flag, .var) {
  dots <- rlang::expr_deparse(dots)
  if (!any(grepl(.var, dots))) {
    flag_is_abort <- identical(.flag, rlang::abort)
    request <- ifelse(flag_is_abort, "must", "should")
    msg <- glue("All expressions passed to `...` {request} refer to `{.var}`.")
    .flag(msg)
  }
  invisible(dots)
}

#' For each expressions in `...`, lowercase the name of a given variable.
#' @noRd
lowercase_var <- function(..., .var) {
  lowercase_each <- function(dots, .var) {
    dots <- gsub(.var, .var, rlang::expr_deparse(dots), ignore.case = TRUE)
    rlang::parse_expr(dots)
  }
  
  lapply(exprs(...), lowercase_each, .var)
}

