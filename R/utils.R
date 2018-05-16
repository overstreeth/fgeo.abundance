# Imports and reexports ---------------------------------------------------

#' @importFrom rlang inform warn abort

# global variables --------------------------------------------------------

# Flag inline helpers as global variables so R CMD check doesn't warn
utils::globalVariables(c(".data", "n", "gx", "gy"))

# Tests -------------------------------------------------------------------

has_class_df <- function(x) {
  any(grepl("data.frame", class(x)))
}
