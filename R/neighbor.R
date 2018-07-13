#' Count and basal area of neighboring stems.
#'
#' Includes all values of `status` and `dbh`. You should pick the values you
#' want before using these functions (e.g. to drop dead stems).
#' 
#' @section Warning:
#' This function has a pending issue by which a dataset with a single row does
#' not result in zero conspecific neighbors
#' (<https://github.com/forestgeo/fgeo.abundance/issues/68>).
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
#' A dataframe with two columns `conspecific` and `heterospecific`, and
#' optionally one extra column for each variable used to group by. The number
#' of rows is as follows:
#' * If `.subset` is `NULL`, the output and `.data` have equal number of rows,
#' both for grouped and ungrouped `.data`.
#' * If `.subset` is not `NULL`, and `.data` is ungrouped, the output and
#' `.subset` have equal number of rows.
#' * If `.subset` is not `NULL`, and `.data` is grouped, the output has a number
#' of rows that equals that of `.subset` multiplied by the number of groups.
#'
#' @examples
#' tree <- fgeo.data::luquillo_tree5_random
#' # Guess `plotdim`
#' count_neighbor(tree, r = 20)
#'
#' count_neighbor(tree, r = 20, plotdim = c(320, 500))
#' 
#' basal_area_neighbor(tree, r = 20, plotdim = c(320, 500))
#' 
#' # Notice how the number of rows of the output varies with the input -------
#' 
#' tree <- tibble::tribble(
#'   ~treeID, ~stemID, ~gx, ~gy, ~tag,   ~sp,  ~dbh, ~status, 
#'      "01",    "01",   5,   5, "01", "sp1",     5,     "A",
#'      "02",    "01",   5,   5, "02", "sp1",     5,     "A",
#'      "03",    "01",   5,   5, "03", "sp2",     5,     "A",
#'      "04",    "01",   5,   5, "04", "sp2",     5,     "A"
#' )
#' 
#' # `.subset = NULL`, ungrouped `.data`
#' count_neighbor(.data = tree, .subset = NULL, r = 20, plotdim = c(320, 500))
#' 
#' # `.subset = NULL`, grouped `.data`
#' count_neighbor(tree, .subset = NULL, r = 20, plotdim = c(320, 500))
#' by_sp <- dplyr::group_by(tree, sp)
#' count_neighbor(.data = by_sp, .subset = NULL, r = 20, plotdim = c(320, 500))
#' 
#' subset <- tibble::tribble(
#'   ~gx, ~gy, ~tag,   ~sp, 
#'   3,   3, "99", "sp1"
#' )
#' 
#' # `.subset` not `NULL`, ungrouped `.data`
#' count_neighbor(.data = tree, .subset = subset, r = 20, plotdim = c(320, 500))
#' 
#' # `.subset` not `NULL`, ungrouped `.data`
#' by_sp <- dplyr::group_by(tree, sp)
#' count_neighbor(.data = by_sp, .subset = subset, r = 20, plotdim = c(320, 500))
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
      # Cancel message "elapsed time ...". Argument `quiet` isn't justifyed.
      neighbours = suppressMessages(
        neighbor_densities(., .subset, r, plotdim, type = type)
      )
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
  
  warn_duplicated_treeid(.data)
  
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
