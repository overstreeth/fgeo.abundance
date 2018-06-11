#' Pick the given or first census (alpha sorted) from a ViewFullTable.
#' 
#' This function performs a very simple action but is valuable mostly for its
#' checks.
#'
#' @param vft Dataframe; particularly a ForestGEO ViewFullTable.
#' @param plot_nm Length-1 character vector of the value of `PlotName` to pick
#'   from `vft`.
#'
#' @return A dataframe.
#' @export
#'
#' @examples
#' vft <- data.frame(PlotName = c("a", "b"), stringsAsFactors = FALSE)
#' pick_plotname(vft)
#' pick_plotname(vft, "b")
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

