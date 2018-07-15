has_class_df <- function(x) {
  any(grepl("data.frame", class(x)))
}

commas <- function(...) {
  paste0(..., collapse = ", ")
}

space <- function(...) {
  paste(unlist(list(...)), collapse = " ")
}

multiple_var <- function(var) {
  force(var)
  var <- tolower(var)
  function(.data) {
    .data <- stats::setNames(.data, tolower(names(.data)))
    .var <- .data[[var]]
    var %in% names(.data) && length(unique(stats::na.omit(.var))) > 1
  }
}

multiple_censusid <- multiple_var("censusid")

multiple_plotname <- multiple_var("plotname")

pull_name <- function(x) gsub(".*\\$", "", x)

