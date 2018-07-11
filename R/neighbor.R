#' Count and basal area of neighbouring stems.
#' 
#' Includes all values of status. You shouild pick the values you want before
#' using these functions (e.g. to drop dead stems).
#'
#' @param .data A Dataframe; particularly  a ForestGEO census table (tree or
#'   stem).
#' @param .subset An optional dataframe giving the (`gx`, `gy`) coordinates of
#'   specific individuals (`tag` and `sp`) for which to count neighbors. For
#'   example, `.subset` may be a subset of only one species; or seedling that
#'   are not part of census `.data`. `.subset` must have columns `gx`, `gy`,
#'   `sp`, and `tag`.
#' @param r Radius.
#' @param plotdim The x and y dimensions of the plot.
#'
#' @return A dataframe.
#' @export
#'
#' @examples
#' tree <- fgeo.data::luquillo_tree5_random
#' neighbor_count(tree, r = 20, plotdim = c(1000, 500))
#' neighbor_basal_area(tree, r = 20, plotdim = c(1000, 500))
neighbor_count <- function(.data, .subset = NULL, r, plotdim) {
  neighbor_densities(.data, .subset, r, plotdim, type = "count")
}

#' @rdname neighbor_count
#' @export
neighbor_basal_area <- function(.data, .subset = NULL, r, plotdim) {
  neighbor_densities(.data, .subset, r, plotdim, type = "basal")
}

