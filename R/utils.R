# Tests -------------------------------------------------------------------

has_class_df <- function(x) {
  any(grepl("data.frame", class(x)))
}
