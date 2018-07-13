has_class_df <- function(x) {
  any(grepl("data.frame", class(x)))
}

commas <- function(...) {
  paste0(..., collapse = ", ")
}

space <- function(...) {
  paste(unlist(list(...)), collapse = " ")
}
