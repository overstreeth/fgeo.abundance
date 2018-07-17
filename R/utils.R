has_class_df <- function(x) {
  any(grepl("data.frame", class(x)))
}

commas <- function(...) {
  paste0(..., collapse = ", ")
}

space <- function(...) {
  paste(unlist(list(...)), collapse = " ")
}

pull_name <- function(x) gsub(".*\\$", "", x)

groups_lower <- function(x) {
  dplyr::grouped_df(x, tolower(dplyr::group_vars(x)))
}

groups_restore <- function(x, ref) {
  dplyr::grouped_df(x, dplyr::group_vars(ref))
}

warn_duplicated_treeid <- fgeo.tool::flag_duplicated_var(warning, treeid)

warn_duplicated_censusid <- fgeo.tool::flag_duplicated_var(warning, censusid)
