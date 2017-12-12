
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



# ctfs

#' Functions used by ctfs
#' 
#' These functions become necessary for functions that come from __cfts__.
#' 
#' @md
#' @name ctfs
#' @keywords internal
#' @importFrom grDevices dev.off graphics.off pdf
#' @importFrom graphics hist lines par plot points text
#' @importFrom stats dnorm median optim qbeta qt quantile rgamma rnorm runif sd
#' @importFrom utils file.edit
NULL



# global variables --------------------------------------------------------

# Flag inline helpers as global variables so R CMD check doesn't warn
utils::globalVariables(c(".data", "n", "gx", "gy"))
