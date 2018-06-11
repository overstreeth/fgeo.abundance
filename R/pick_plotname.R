# TODO: Move to fgeo.base

#' Pick a single plot from a ViewFullTable.
#' 
#' This is a convenient wrapper around `dplyr::filter()` warns if multiple
#' plots are detected and detaults to pick the first plot in alphabetical order.
#'
#' @param vft Dataframe; particularly a ForestGEO ViewFullTable.
#' @param plot_nm Length-1 character vector of the value of `PlotName` to pick
#'   from `vft`.
#'
#' @return A dataframe.
#' @export
#'
#' @examples
#' 
pick_plotname <- function(vft, plot_nm = NULL) {
  stopifnot(is.data.frame(vft))
  fgeo.base::check_crucial_names(vft, "PlotName")

  plots <- sort(unique(vft$PlotName))
  if (is.null(plot_nm)) plot_nm <- plots
  
  valid_plot <- sort(unique(vft$PlotName))
  if (!all(plot_nm %in% valid_plot)) {
    stop("`PlotName = ", plot_nm, "` wasn't detected.", call. = FALSE)
  }
  
  message("Using: ", plot_nm[[1]], ".")
  vft[vft$PlotName == plot_nm[[1]], , drop = FALSE]
}


