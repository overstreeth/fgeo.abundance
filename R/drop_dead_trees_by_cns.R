#' Drop dead trees by census from a ViewFullTable.
#' 
#' If you have a ViewFullTable and want to compute some summary on exclusively
#' alive trees, you can't just drop dead trees across the entire ViewFullTable. 
#' If you do that you'll loose trees (tags) which were alive in earlier
#' censuses. This function helps you solve that problem by (1) grouping the
#' data by census; (2) calculating the status of each tree based on the status
#' of its stem(s); (3) dropping dead trees (i.e. trees which all stems are
#' dead).
#'  
#' @param vft A dataframe; particularly a ForestGEO ViewFullTable.
#' @param status_d String indicating dead stems in the column `Status` of `vft`
#'   (commonly "dead" or "D").
#' @inheritParams fgeo.tool::filter_status
#' @param .valid_status String giving possible values of `Status`.
#'
#' @family functions for fgeo vft.
#' @family functions by census.
#' 
#' @return A dataframe.
#' @export
#'
#' @examples
#' vft <- data.frame(
#'   TreeID = c("0001", "0001", "0002", "0002"),
#'   PlotName = "p",
#'   Status = c("dead", "dead", "alive", "alive"),
#'   DBH = c(NA, NA, 10, 100),
#'   ExactDate = c("2000-01-01", "2001-01-01", "2000-01-01", "2001-01-01"),
#'   PlotCensusNumber = c(1, 2, 1, 2),
#'   CensusID = c(1, 2, 1, 2),
#'   Genus = c("A", "A", "B", "B"),
#'   SpeciesName = c("a", "a", "b", "b"),
#'   Family = "f",
#'   stringsAsFactors = FALSE
#' )
#' vft
#' 
#' drop_dead_trees_by_cns(vft)
drop_dead_trees_by_cns <- function(vft, 
                                   status_d = "dead",
                                   .valid_status = c(
                                   "dead", "alive", "broken below", "missing")
                                   ) {
  filter_tree_status_by_census(
    vft = vft, .status = status_d, exclude = TRUE, .valid_status = .valid_status
  )
}

# Add `status_tree` by census and pick or drop alive or dead trees.
filter_tree_status_by_census <- function(vft, .status, exclude, .valid_status) {
  stopifnot(length(.status) == 1, .status %in% c("dead", "alive"))
  # Other crucial names are checked downstream
  fgeo.base::check_crucial_names(vft, "PlotCensusNumber")
  
  sane <- sanitize_status(vft, .valid_status)
  
  message("Calculating tree-status (from stem `Status`) by `PlotCensusNumber`.")
  with_status_tree <- sane %>% 
    dplyr::group_by(.data$PlotCensusNumber) %>% 
    fgeo.tool::add_status_tree(status_a = "alive", status_d = "dead") %>% 
    dplyr::ungroup()
  
  filtering <- ifelse(exclude, "Dropping", "Picking")
  message(filtering, " trees which status is ", .status, ".")
  fgeo.tool::filter_status(
    with_status_tree, wood = "tree", .status = .status, exclude = exclude
  )
}

sanitize_status <- function(vft, .valid_status) {
  vft_warned <- inform_if_bad_status(vft, .valid_status)
  fix_status_if_bad_or_err(vft_warned, .valid_status)
}
