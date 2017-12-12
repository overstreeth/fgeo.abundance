
# Global functions --------------------------------------------------------

# rlang

# built with `usethis::use_tidy_eval()``

#' Tidy eval helpers
#'
#' These functions provide tidy eval-compatible ways to capture
#' symbols (`sym()`, `syms()`, `ensym()`), expressions (`expr()`,
#' `exprs()`, `enexpr()`), and quosures (`quo()`, `quos()`, `enquo()`).
#' To learn more about tidy eval and how to use these tools, read
#' <http://rlang.tidyverse.org/articles/tidy-evaluation.html>
#'
#' @md
#' @name tidyeval
#' @keywords internal
#' @aliases          quo quos enquo sym syms expr exprs enexpr quo_name
#' @importFrom rlang quo quos enquo sym syms expr exprs enexpr quo_name
#' @export           quo quos enquo sym syms expr exprs enexpr quo_name
NULL

# global variables --------------------------------------------------------

# Flag inline helpers as global variables so R CMD check doesn't warn
utils::globalVariables(c(".data", "n", "gx", "gy"))
