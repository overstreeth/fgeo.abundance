#' Abundance.
#' 
#' * Similar to [dplyr::count()] but simpler. It keeps only main argument.
#' * Supports groups only via group_by() not via `...`
#' * Returns 0 when fed with a 0-row dataframe.
#' 
#' [fgeo.abundance::abundance()] versus ctfs::abundance():
#' * When fed with a 0-row dataframe, 
#'     * `ctfs::abundance` returns a dataframe with zero rows and zero columns. 
#'     * [fgeo.abundance::abundance()] returns a dataframe column `n = 0`.
#' * `ctfs::abundance` handles only two grouping variables, via arguments
#' `split1` and `split2`.
#' * [fgeo.abundance::abundance()] handles any number of grouping variables via
#' [dplyr::group_by()].
#' 
#' * `ctfs::abundance` returns a list, which is difficult to visualize and use.
#' * [fgeo.abundance::abundance()] returns a dataframe subclass tibble, which
#' prints well even with large datasets and works well with a larger number of
#' tools for data analysis (e.g. __ggplot2__ and __dplyr__).
#' 
#' @section Flags:
#' Warns if it detects:
#' * duplicated values of treeis.
#' * multiple values of censusid.
#' * multiple values of plotname.
#' 
#' @section Warning:
#' To pick specific rows (e.g. to pick "alive" stems or `dbh` within some range)
#' you should use functions such as `[`, [subset()], or [dplyr::filter()]
#' _before_ using `abundance()`. (Notice the difference with `ctfs::abundance`,
#' which includes arguments to deal with `dbh` and `status`).
#' 
# abundance <- function(x) {
#   low_nms <- groups_lower(set_names(x, tolower))
#   warn_if_needed_plotname_censusid(low_nms)
#   restore_input_names_output_groups(dplyr::count(low_nms), x)
# }
with_anycase_group_df <- function(.f, .warn) {
  function(x) {
    low_nms <- groups_lower(set_names(x, tolower))
    # FIXME: basal_area2() should warn if duplicated stemid and not treeid
    .warn(low_nms)
    warn_if_needed_plotname_censusid(low_nms)
    restore_input_names_output_groups(.f(low_nms), x)
  }
}
abundance <- with_anycase_group_df(dplyr::count, warn_if_needed_treeid)
basal_area2 <- with_anycase_group_df(ba_df, warn_if_needed_stemid)


T
# Only if data contains specific `name`s.
warn_if_needed_plotname_censusid <- function(.x) {
  warn_if_has_var(
    .x, name = "censusid", predicate = is_multiple,
    problem = "Multiple", hint = "Do you need to group by censusid?"
  )
  warn_if_has_var(
    .x, name = "plotname", predicate = is_multiple,
    problem = "Multiple", hint = "Do you need to pick a single plot?"
  )
  
  invisible(.x)
}

warn_if_needed_treeid <- function(.x) {
  warn_if_has_var(
    .x, name = "treeid", predicate = is_duplicated,
    problem = "Duplicated", hint = "Do you need to pick main stems?"
  )
  invisible(.x)
}

warn_if_needed_stemid <- function(.x) {
  warn_if_has_var(
    .x, name = "stemid", predicate = is_duplicated,
    problem = "Duplicated", hint = "Do you need to pick largest `hom` values?"
  )
  invisible(.x)
}

warn_if_has_var <- function(.x, name, predicate, problem, hint) {
  if (hasName(.x, name)) {
    msg <- glue("`{name}`: {problem} values were detected. {hint}")
    flag_if_group(.x, name, predicate, warn, msg)
  }
}
