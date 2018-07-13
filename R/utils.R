has_class_df <- function(x) {
  any(grepl("data.frame", class(x)))
}

commas <- function(...) {
  paste0(..., collapse = ", ")
}

space_dots <- function(...) {
  paste(unlist(list(...)), collapse = " ")
}


