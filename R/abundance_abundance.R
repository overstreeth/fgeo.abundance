#' Basal area of trees.
#' 
#' * `ba()`: calculates the individual basal areas in square meters for all
#'   `dbh`-values submitted.
#' * `ba_sum()`: calculates the basal area summed over all submitted `dbh` values
#'   (after removing NAs).
#' 
#' @template dbh
#' @template dbhunit
#' @template mindbh
#' 
#' @template author_condit
#' 
#' @return
#'   * `ba()`: A vector of basal area values of same length as the submitted 
#'     vector of dbhs.
#'   * `ba_sum()`: A number giving the the basal area summed over all submitted
#'     dbhs.
#'
#' @name basal_area
#' @aliases ba_sum
#'
#' @examples 
#' # Takes NAs and throws informative warning
#' (dbh <- c(seq(5, 50, by = 5), NA))
#' ba(dbh, dbhunit = "cm")  # default dbhunit is milimeters ("mm")
#' 
#' # (From now on, suppressing warnings to avoid distraction)
#' 
#' # Filter stems at or over a minimum dbh and sum them, all in one step
#' suppressWarnings(
#'   ba_sum(dbh, mindbh = 30)
#' )
#' 
#' # Same; a bit longer but may be easier to understand what is going on
#' over_mindbh <- dbh[dbh >= 30]
#' suppressWarnings(
#'   sum(ba(over_mindbh), na.rm = TRUE)
#' )

#' @rdname basal_area
#' @export
ba <- function(dbh, dbhunit = "mm") {
  validate_ba(dbh = dbh, dbhunit = dbhunit)

  if (dbhunit == "cm") {return(pi * (dbh / 200) ^ 2)}
  pi * (dbh / 2000) ^ 2
}

#' @rdname basal_area
#' @export
ba_sum <- function(dbh, dbhunit = "mm", mindbh = 10) {
  validate_ba(dbh = dbh, dbhunit = dbhunit)
  if (!is.null(mindbh)) {
    assertive::assert_is_a_number(mindbh, severity = "warning")
    assertive::assert_all_are_positive(mindbh)
  }
  
  if (!is.null(mindbh)) {
    dbh <- dbh[dbh >= mindbh]
  }
  if (length(dbh) == 0) {
    return(0)
  }
  sum(ba(dbh, dbhunit = dbhunit), na.rm = TRUE)
}



#' Abundance measured in number of stems, basal area or above ground biomass.
#'
#' @details
#' * `abundance()` calculates total abundance or basal area, dividing data with
#'   1 or 2 categorical variables passed to `split1` and `split2`. The length
#'   of `split1` and `split2` must match exactly the number of rows in 
#'   `censdata` (so one per tree or stem). `split1` should be the one with the
#'   most categories (for instances, `split1 = species`, `split2 = 
#'   dbhcategory`).
#'   
#'   Each element of the returned list is an array of one or two dimensions, 
#'   depending on how many split variables were submitted: each of the 
#'   dimensions of the array handles one of the categorical variables.
#' 
#'   For basal area, pass a stem table to `censdata`; For abundance, use either 
#'   the stem or full table to count stems or trees, respectively.
#'
#' * `abundanceperquadrat()` wraps `abundance()` to find abundance, basal area,
#'   or agb of every species per square quadrat of any size.
#'
#'   If the plot dimensions is not an exact multiple of the quadrat size, then a
#'   strip at the upper edge of the plot (north and east if plot is on cardinal
#'   directions) is omitted. For example, if `gridsize = 40` and `plotdim = 500`,
#'   then there are an extra 20 meters at the upper boundary omitted from the
#'   calculations.
#'
#'   The array of abundances per quadrat is useful for similarity, counting
#'   species and stems per quadrat, etc.
#'   
#' * `abundance_match_census_habitat()` wraps `abundanceperquad()` ensuring
#'   consistent `gridsize` and `plotdim` via [extract_gridsize()] and
#'   [extract_plotdim()].
#'
#' @section Acknowledgement:
#' Thanks to Gabriel Arellano for suggesting that the match plot dimensions
#' between habitat data and census data should be enforced programatically.
#'
#' @template censdata
#' @template type
#' @template alivecode
#' @template mindbh
#' @template dbhunit
#' @param split1,split2 A vector of categories, one per individual. See details.
#' @template plotdim
#' @template gridsize
#' @template habitats
#' 
#' @return
#' * `abundance()` and `abundanceperquad()` return a named list of two elements:
#'   1. either `abund` or `ba`
#'   1. `meandate` (mean measurement date for all individuals in each category).
#'
#' * `abundance_match_census_habitat()`: A dataframe giving the abundance per
#'   quadrat.
#' 
#' @name abundance
#' @seealso [ba], [ba_sum]
#' @author `abundance() and `abundanceperquad()` were developed by Richard 
#'   Condit.
#'
#' @examples
#' # Small subsets of data for examples
#' library(dplyr)
#' tree_mini <- dplyr::sample_n(bci::bci12full5, 1000)
#' stem_mini <- bci::bci12stem5[1:1000, ]
#' 
#' 
#' 
#' (tree_abundance <- abundance(tree_mini, mindbh = 10))
#' (stem_abundance <- abundance(stem_mini, mindbh = 10))
#' 
#' stem_basal_area_by_sp <- abundance(
#'   stem_mini,
#'   type = "ba",
#'   mindbh = 10,
#'   split1 = stem_mini$sp
#' )
#' lapply(stem_basal_area_by_sp, head)
#' 
#' 
#' 
#' tree_n <- abundanceperquad(
#'   tree_mini,
#'   plotdim = c(1000, 500),
#'   gridsize = 100,
#'   type = "abund"
#' )
#' str(tree_n$abund, list.len = 10)
#' 
#' # Matching `gridsize` and `plotdim` between census data and habitat data
#' abund <- abundance_match_census_habitat(tree_mini, bci::bci_habitat)
#' str(abund, list.len = 6)
#' @export
#' @rdname abundance
abundance <- function(censdata,
                      type = "abund",
                      alivecode = c("A"),
                      mindbh = NULL,
                      dbhunit = "mm",
                      split1 = NULL,
                      split2 = NULL) {
  validate_abundance(
    censdata, type, alivecode, mindbh, dbhunit, split1, split2
  )
  
  if (is.null(split1)) {
    split1 <- rep("all", dim(censdata)[1])
  }
  if (is.null(split2)) {
    split2 <- rep("all", dim(censdata)[1])
  }

  if (!is.null(mindbh)) {
    inc <- censdata$dbh >= mindbh
  } else {
    inc <- rep(TRUE, length(censdata$dbh))
  }
 
  alive <- rep(FALSE, length(censdata$dbh))
  for (i in 1:length(alivecode)) {
    alive[censdata$status == alivecode[i]] <- TRUE
  }

  class1 <- sort(unique(split1))
  class2 <- sort(unique(split2))
  groupvar <- list(split1[alive & inc], split2[alive & inc])
  
  if (type == "abund") {
    abund <- tapply(censdata$dbh[alive & inc], groupvar, length)
  }
  if (type == "ba") {
    abund <- tapply(
      censdata$dbh[alive & inc],
      groupvar,
      ba_sum, mindbh = mindbh, dbhunit = dbhunit
    )
  } 
  if (type == "agb") {
    abund <- tapply(
      censdata$agb[alive & inc], 
      groupvar, 
      sum, na.rm = TRUE
    )
  }

  meandate <- tapply(censdata$date[alive & inc], groupvar, mean, na.rm = TRUE)
  
  abund_filled <- fill_dimension(abund, class1, class2)
  meandate_filled <- fill_dimension(meandate, class1, class2, fill = NA)
  
  result <- list(abund = abund_filled, meandate = meandate_filled)
  if (type == "ba") {
    names(result)[1] = "ba"
  }
  if (type == "agb") {
    names(result)[1] = "agb"
  }
  result
}






#' @export
#' @rdname abundance
abundanceperquad <- function(censdata,
                             mindbh = 10,
                             plotdim = c(1000, 500),
                             gridsize = 100,
                             type = 'abund',
                             dbhunit = "mm") {
  # All arguments are validated inside to_id_gxg() and abundance()
  
  sp <- censdata$sp

  quadno <- to_id_gxgy(
    censdata$gx,
    censdata$gy,
    gridsize = gridsize,
    plotdim = plotdim
  )
 
  result <- abundance(
    censdata,
    type = type,
    mindbh = mindbh,
    dbhunit = dbhunit,
    split1 = sp,
    split2 = quadno
  )
  
  allspp <- unique(censdata$sp)
  maxquad <- floor(plotdim[1] / gridsize) * floor(plotdim[2] / gridsize)
  allquad <- 1:maxquad
  
  condition1 <- dim(result[[type]])[1] < length(allspp)
  condition2 <- dim(result[[type]])[2] < length(allquad)
  if (condition1 | condition2) {
    result[[type]] <- fill_dimension(
      .data = result[[type]],
      class1 = allspp,
      class2 = allquad,
      fill = 0
    )
  }

  result
}

#' @rdname abundance
#' @export
abundance_match_census_habitat <- function(censdata, habitats) {
  validate_abundance_match_census_habitat(censdata, habitats)

  abundanceperquad(
    censdata,
    gridsize = extract_gridsize(habitats),
    plotdim = extract_plotdim(habitats)
  )$abund
}

#' Extract plot dimensions from habitat data.
#'
#' @template habitats
#' @name extract_from_habitat
#'
#' @return
#' * [extract_plotdim()]: `plotdim` (vector of length 2);
#' * [extract_gridsize()]: `gridsize` (scalar).
#'
#' @examples
#' extract_plotdim(bci::bci_habitat)
#' extract_gridsize(bci::bci_habitat)
NULL

#' @export
#' @rdname extract_from_habitat
extract_gridsize <- function(habitats) {
  assert_are_names_matching(habitats, c("x", "y"))

  grid_x <- difference_among_grid_steps(habitats$x)
  grid_y <- difference_among_grid_steps(habitats$y)
  gridsize <- unique(grid_x, grid_y)
  gridsize
}
#' @export
#' @rdname extract_from_habitat
extract_plotdim <- function(habitats) {
  assert_are_names_matching(habitats, c("x", "y"))

  gridsize <- extract_gridsize(habitats)
  plotdim <- unlist(
    lapply(habitats[c("x", "y")], function(.x){max(.x) + gridsize})
  )
  unname(plotdim)
}

#' From x and y columns of habitat data, get difference between grid steps.
#'
#' @param habitat_x_or_y Column x or y of habitat data, e.g. bci::bci_habitat$x.
#'
#' @return A non negative scalar
difference_among_grid_steps <- function(habitat_x_or_y) {
  assertive::assert_is_non_empty(habitat_x_or_y)
  assertive::assert_is_vector(habitat_x_or_y)
  assertive::assert_all_are_non_negative(habitat_x_or_y)

  grid_steps <- unique(habitat_x_or_y)
  difference_among_grid_steps <- unique(diff(grid_steps))

  difference_among_grid_steps
}



# validate ----------------------------------------------------------------

validate_ba <- function(dbh, dbhunit) {
  assertive::assert_is_numeric(dbh)
  assertive::assert_any_are_matching_regex(dbhunit, pattern = "^mm$|^cm$")
  assertive::assert_all_are_positive(dbh, na_ignore = TRUE)
  assertive::assert_all_are_not_na(dbh, severity = "warning")
}

validate_abundance <- function(censdata,
                               type = "abund",
                               alivecode = c("A"),
                               mindbh = NULL,
                               dbhunit = "mm",
                               split1 = NULL, 
                               split2 = NULL) {
  assert_are_names_matching(censdata, c("dbh", "status", "agb", "date"))
  assertive::assert_any_are_matching_regex(type, "^abund$|^ba$|^agb$")
  if (!is.null(mindbh)) {
    assertive::assert_is_numeric(mindbh)
  }
  assertive::assert_any_are_matching_regex(dbhunit, "mm|cm")

  anchored_names <- paste0("^", colnames(censdata), "$")
  censdata_names <- paste(anchored_names, collapse = "|")
  
  if (!is.null(split1)) {
    assertive::assert_is_of_length(split1, nrow(censdata))
  }
  
  if (!is.null(split2)) {
    assertive::assert_is_of_length(split2, nrow(censdata))
  }
}

# Reject obviously incorrect data
validate_abundance_match_census_habitat <- function(censdata, habitats) {
  assert_are_names_matching(censdata, c("tag", "sp"))
  assert_are_names_matching(habitats, c("x", "y"))
}
