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
#' @aliases abundance_neighbor
#'
#' @return A dataframe.
#'
#'
#' @examples
#' tree <- fgeo.data::luquillo_tree5_random
#' # Guess `plotdim`
#' count_neighbor(tree, r = 20)
#'
#' count_neighbor(tree, r = 20, plotdim = c(320, 500))
#' basal_area_neighbor(tree, r = 20, plotdim = c(320, 500))
#' @name neighbor
NULL

neighbor <- function(type) {
  force(type)
  function(.data,
             .subset = NULL,
             r,
             plotdim = NULL) {
    check_neighbor(.data, .subset)

    plotdim <- plotdim %||% fgeo.base::guess_plotdim(.data)
    out <- dplyr::do(
      .data,
      neighbours = neighbor_densities(., .subset, r, plotdim, type = type)
    )
    tidyr::unnest(out)
  }
}

#' @rdname neighbor
#' @export
count_neighbor <- neighbor(type = "count")

#' @rdname neighbor
#' @export
basal_area_neighbor <- neighbor(type = "basal")

check_neighbor <- function(.data, .subset) {
  stopifnot(is.data.frame(.data))
  if (!is.null(.subset)) stopifnot(is.data.frame(.subset))

  crucial_sub <- c("gx", "gy", "tag", "sp")
  crucial_data <- c(crucial_sub, "dbh", "status")
  prepend_crucial_nm_msg(.data, crucial_data, "Invalid `.data`. ")
  if (!is.null(.subset)) {
    prepend_crucial_nm_msg(.subset, crucial_sub, "Invalid `.subset`. ")
  }
  invisible(.data)
}

prepend_crucial_nm_msg <- function(x, nm, msg) {
  tryCatch(
    check_crucial_names(x, nm),
    error = function(e) {
      e$message <- paste0(msg, e$message)
      stop(e)
    }
  )
}
