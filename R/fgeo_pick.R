#' Pick useful observations from a ViewFullTable.
#'
#' @inheritParams pick_plotname
#' @param valid_status String giving possible values of `Status`.
#' @inheritParams fgeo.tool::filter_status
#'
#' @return A dataframe.
#' @examples
#' \dontrun{
#' pick_fgeo(vft)
#' }
#' @noRd
pick_one_plot <- function(vft, 
                          plot_nm = sort(unique(vft$PlotName)), 
                          valid_status = c(
                            "dead", "alive", "broken below", "missing"
                          ), 
                          .status = "dead", 
                          exclude = TRUE) {
  crucial <- c("PlotName", "Status", "DBH", "ExactDate", "PlotCensusNumber")
  fgeo.base::check_crucial_names(vft, crucial)
  
  vft_one <- pick_plotname(vft, plot_nm)
  
  vft_over_one <- fgeo.base::pick_dbh_min(vft_one, 1)
  dbh_range <- paste(range(vft_one$DBH, na.rm = TRUE), collapse = "-")
  message("Detected `DBH` values ranging ", dbh_range)
  
  
  # Status
  
  # sanitize_status()
  vft_warned <- inform_if_bad_status(vft_over_one, valid_status)
  vft_good_status <- fix_status_if_bad_or_err(vft_warned, valid_status)
  
  # add tree status by census
  message("Calculating tree-status (from stem `Status`) by `PlotCensusNumber`.")
  stts_tree <- vft_good_status %>% 
    dplyr::group_by(PlotCensusNumber) %>% 
    fgeo.tool::add_status_tree(status_a = "alive", status_d = "dead") %>% 
    dplyr::ungroup()
  
  # filter status
  filtering <- ifelse(exclude, "Dropping", "Picking")
  message(filtering, " rows where `Status = ", .status, "`.")
  not_dead <- fgeo.tool::filter_status(
    stts_tree, wood = "tree", .status = .status, exclude = exclude
  )
  
  # sanitize_dbh()
  not_na_dbh <- drop_if_missing_dbh(not_dead)
  drop_if_missing_dates(not_na_dbh)
}





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
pick_plotname <- function(vft, plot_nm = sort(unique(vft$PlotName))) {
  if (length(plot_nm) > 1) {
    message(
      "Detected more than one `PlotName`: (", commas(plot_nm), ")\n",
      "* Using ", plot_nm[[1]]
    )
  }
  dplyr::filter(vft, PlotName == plot_nm[[1]])
}



inform_if_bad_status <- function(vft, status_arg) {
  status_ok <- all(sort(unique(vft$Status)) %in% status_arg)
  if (!status_ok) {
    message(
      "Unique values of column `Status` and argument `valid_status` ", 
      "should match:\n",
      "* Status col: ", commas(sort(unique(status_arg))), ".\n",
      "* valid_status arg: ", commas(sort(unique(vft$Status))), "."
    )
  }
  invisible(vft)
}

identical_status_levels <- function(status_col, status_arg) {
  identical(sort(unique(status_col)), sort(unique(status_arg)))
}



fix_status_if_bad_or_err <- function(vft, status_arg) {
  status_ok <- all(sort(unique(vft$Status)) %in% status_arg)
  if (!status_ok) {
    message("Fixing status automatically.")
    vft <- fix_bad_status(vft, vft$Status, status_arg)
    
    tryCatch(
      testthat::expect_silent(inform_if_bad_status(vft, status_arg)),
      error = function(cond) {
        stop(
          "Tried but failed to fix status automatically.\n",
          "* Fix `Status` manually and retry.", 
          call. = FALSE
        )
      },
      warning = function(cond) "Failed to fix status automatically."
    )
  }
  invisible(vft)
}

fix_bad_status <- function(vft, status_col, status_arg) {
  vft$Status <- sub("^.*dead.*$", "dead", vft$Status)
  vft$Status <- sub("^.*alive.*$", "alive", vft$Status)
  vft
}



drop_if_missing_dbh <- function(dfm) {
  missing_dbh <- is.na(dfm$DBH)
  if (any(missing_dbh)) {
    warning(
      "Dropping ", sum(missing_dbh), " rows with missing `DBH` values.",
      call. = FALSE
    )
  }
  dplyr::filter(dfm, !is.na(DBH))
}



drop_if_missing_dates <- function(x) {
  missing_dates <- is.na(x$ExactDate)
  if (any(missing_dates)) {
    warning("Detected and ignoring missing dates.")
  }
  x <- x[!missing_dates, , drop = FALSE]
  invisible(x)
}



mean_years <- function(vft) {
  years <-  vft %>% 
    dplyr::group_by(PlotCensusNumber) %>% 
    dplyr::summarise(
      year = round(mean(lubridate::year(ExactDate), na.rm = TRUE))
    ) %>% 
    unique() %>% 
    dplyr::arrange(PlotCensusNumber) %>% 
    dplyr::ungroup()
  dplyr::left_join(vft, years, by = "PlotCensusNumber") %>% 
    dplyr::mutate(species = paste(Genus, SpeciesName)) %>% 
    dplyr::arrange(year)
}



fgeo_abundance <- function(vft) {
  vft %>% 
    drop_if_na("year") %>% 
    dplyr::count(species, Family, year) %>% 
    tidyr::spread(year, n, fill = 0) %>% 
    dplyr::arrange(species, Family)
}

# TODO: Move to fgeo.base.
# TODO: write a data.frame and vector method.
drop_if_na <- function(dfm, x) {
  .x <- dfm[[x]]
  missing <- is.na(.x)
  if (any(missing)) {
    warning(
      "Dropping ", sum(missing), " rows with missing `", x, "` values.", 
      call. = FALSE
    )
  }
  dfm[!missing, , drop = FALSE]
}

fgeo_basal_area <- function(vft) {
  vft %>% 
    dplyr::group_by(species, Family, year) %>%
    basal_area(dbh = DBH) %>% 
    dplyr::arrange(species, Family, year) %>% 
    dplyr::ungroup() %>% 
    tidyr::spread(year, basal_area, fill = 0)
}



# Utils -------------------------------------------------------------------

commas <- function(...) {
  paste0(..., collapse = ", ")
}

