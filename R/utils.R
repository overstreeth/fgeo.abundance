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

#' Get the correct grouping variables.
#' 
#' This funciton is useful when working inside a function that works with
#' lowercase variable and group names. The output of such function needs to may
#' have the correct grouping variables but with the wrong case. This function
#' outputs a sting of the grouping variable in `x` with the case of `y`.
#'
#' @param x A dataframe which groups are ok but lowercase.
#' @param y A reference dataframe which gropus are not ok but have correct case.
#' @keywords internal
#' @noRd
#' 
#' @examples 
#' 
#' out <- dplyr::grouped_df(tibble::tibble(x = 1, y = 1, z = 1), c("x", "y"))
#' out
#' 
#' ref <- dplyr::grouped_df(rlang::set_names(out, toupper), c("X"))
#' group_vars_restore(out, ref)
group_vars_restore <- function(x, y) {
  in_ref <- fgeo.base::detect_insensitive(
    dplyr::group_vars(x), 
    dplyr::group_vars(y)
  )
  
  fgeo.base::extract_insensitive(
    dplyr::group_vars(x), 
    dplyr::group_vars(y)
  )
}
