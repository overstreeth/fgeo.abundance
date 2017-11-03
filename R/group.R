group <- function(x, group_by, only_alive) {
  assertive::assert_is_non_empty(x)
    
  if (only_alive) {
    x <- x[x$status == "A", ]
  }
  
  if (is.null(group_by)) {
    stop("group_by can't be NULL.")
  }
  
  # Convert from character string to bare names (https://goo.gl/kPqMUk)
  parsed_groups <- lapply(group_by, rlang::parse_quosure)
  dplyr::group_by(x, rlang::UQS(parsed_groups))
}
