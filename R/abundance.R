# ***spatial; quadfunc*** -------------------------------------------------

#' Convert to quadrat indices.
#' 
#' These functions output quadrat indices from either row and column numbers
#' ([to_id_rowcol]), or gx and gy coordinates ([to_id_gxgy]]). They replace
#' [ctfs::rowcol.to.index] and [ctfs::gxgy.to.index], but these old names are 
#' kept as aliases to help users find the newer names.
#' 
#' @param .row,.col Row and column number.
#' @template gx_gy
#' @template gridsize
#' @template plotdim
#' 
#' @return A numeric vector of indices.
#' 
#' @aliases rowcol.to.index gxgy.to.index
#' @template author_condit
#' @family converter functions
#' @name to_id
#' @examples
#' library(dplyr)
#' mini_cns <- bci::bci12full7 %>% 
#'   select(gx, gy) %>% 
#'   sample_n(10)
#' to_id_gxgy(mini_cns$gx, mini_cns$gy, gridsize = 20, plotdim = c(1000, 500))
#' 
#' to_id_rowcol(.row = 1:10, .col = 6:15, gridsize = 20, plotdim = c(1000, 500))
NULL

#' @rdname to_id
#' @export
to_id_rowcol <- function(.row, .col, gridsize, plotdim) {
  validate_to_id_rowcol(.row = .row, .col = .col, gridsize = gridsize,
    plotdim = plotdim)
  
  badrc <- (.row <= 0 | .col <= 0 | .row > plotdim[2] / gridsize |
      .col > plotdim[1] / gridsize)
  diff <- 1
  .row_minus_diff <- .row - diff
  .col_minus_diff <- .col - diff
  maxrow <- floor(plotdim[2] / gridsize)
  index <- .col_minus_diff * maxrow + .row_minus_diff + diff
  if (length(badrc[badrc > 0])) {index[badrc] <- NA}
  return(index)
}

validate_to_id_rowcol <- function(.row, .col, gridsize, plotdim) {
  validate_gridsize_plotdim(gridsize = gridsize, plotdim = plotdim)
  validate_row_col(.row = .row, .col = .col)
  warn_bad_arg_to_id_rowcol(.row = .row, .col = .col, gridsize = gridsize,
    plotdim = plotdim)
}

# xxxxxxxxxxxxxxx ------------

# context("validate_to_id_rowcol")
# 
# test_that("from to_id_rowcol(), correctly calls functions that err or warn", {
  expect_error(
    to_id_gxgy(1:3, 1:3, rep(NA_real_, 3), c(1000, 500))
  )
  expect_error(
    to_id_gxgy(1:3, 1:3, rep(NA_real_, 3), c(1000, 500))
  )
# })







#' @rdname to_id
#' @export
to_id_gxgy <- function(gx, gy, gridsize, plotdim) {
  validate_gridsize_plotdim(gridsize = gridsize, plotdim = plotdim)
  warn_bad_arg_to_id_gxgy(gx = gx, gy = gy, gridsize = gridsize, 
  plotdim = plotdim)
  
  badgxgy <- (gx < 0 | gy < 0 | gx >= plotdim[1] | gy >= plotdim[2] |
        is.na(gx) | is.na(gy))
  .col_adjusted <- 1 + floor(gx / gridsize)
  .row_adjusted <- 1 + floor(gy / gridsize)
  if (length(badgxgy[badgxgy > 0])) {
    .col_adjusted[badgxgy] <- NA
    .row_adjusted[badgxgy] <- NA
  }
  return(to_id_rowcol(.row_adjusted, .col_adjusted, gridsize, plotdim))
}

validate_gridsize_plotdim <- function(gridsize, plotdim) {
  assertive::assert_all_are_not_na(gridsize)
  assertive::assert_all_are_not_na(plotdim)
  assertive::assert_all_are_positive(gridsize)
  assertive::assert_all_are_positive(plotdim)
}

# Choosingn to warn (not err) because the intention of the function is unclear.
validate_row_col <- function(.row, .col) {
  assertive::assert_all_are_not_na(.row, severity = "warning")
  assertive::assert_all_are_not_na(.col, severity = "warning")
}

# These functions throws warning if bad arguments are detected. Errors are not
# appropriate because the bad arguments are meaningful inside these functions.

warn_bad_arg_to_id_gxgy <- function(gx, gy, gridsize, plotdim) {
  assertive::assert_all_are_positive(gx, severity = "warning")
  assertive::assert_all_are_positive(gy, severity = "warning")
  assertive::assert_all_are_false(
    gx >= plotdim[1], severity = "warning"
  )
  assertive::assert_all_are_false(
    gy >= plotdim[2], severity = "warning"
  )
  assertive::assert_all_are_not_na(gx, severity = "warning")
  assertive::assert_all_are_not_na(gy, severity = "warning")
}

warn_bad_arg_to_id_rowcol <- function(.row, .col, gridsize, plotdim) {
  assertive::assert_all_are_non_negative(.row, severity = "warning")
  assertive::assert_all_are_non_negative(.col, severity = "warning")
  assertive::assert_all_are_false(
    .row > plotdim[2] / gridsize, severity = "warning"
  )
  assertive::assert_all_are_false(
    .col > plotdim[1] / gridsize, severity = "warning"
  )
}



# ***utilities; utilities*** ----------------------------------------------

#' Fill 2 dimensional data with custom value.
#'
#' This function fills out an array of 2 dimensions with custom values for extra
#' columns and rows as named in `class1` and `class2`. If a column or row is
#' missing, it will be filled with the value given by fill. It is useful for
#' results of table or tapply when some elements had no records.
#' 
#' @param .data A two-dimension array, matrix or data.frame.
#' @param class1,class2 A vector of the same length  of `.data` or longer.
#' @param fill A numeric value to fill missing values with.
#'
#' @aliases fill.dimension
#' 
#' @export
#' @examples 
#' class1 <- 1:2
#' class2 <- letters[1:5]
#' fill = 0
#' .data <- array(
#'   data = c(1, 2, NA),
#'   dim = c(2, 4),
#'   dimnames = list(1:2, letters[1:4])
#' )
fill_dimension <- function(.data, class1, class2, fill = 0) {
  validate_data_class1_class2_fill(.data, class1, class2, fill)
  
  result <- data.frame(
    matrix(fill, nrow = length(class1), ncol = length(class2))
  )
  rownames(result) <- class1
  colnames(result) <- class2

  result[rownames(.data), colnames(.data)] <- .data
  result[is.na(result)] <- fill
  result
}

validate_data_class1_class2_fill <- function(.data, class1, class2, fill) {
  # let class1 = NULL pass this test
  if (!is.null(class1)) {
    assertive::assert_any_are_not_na(class1, severity = "warning")
  }
  assertive::assert_is_of_length(dim(.data), 2, severity = "warning")
  assertive::assert_any_are_matching_regex(
    class(.data), "matrix|data.frame", severity = "warning"
  )
  class2_is_not_shorter_than_n_cols_of_data <- length(class2) >= ncol(.data)
  assertive::assert_is_identical_to_true(
    class2_is_not_shorter_than_n_cols_of_data, severity = "stop"
  )
}






#' Assert important names match names in data.
#'
#' @param .data Data
#' @param match  Names to match with names of `.data`.
#' @param ... Arguments passed to [assertive::assert_all_are_matching_regex()],
#'   e.g. `severity = "stop"` (default), `severity = "warning"`.
#'
#' @return Nothing, warning, error, etc. See 
#'   [assertive::assert_all_are_matching_regex()].
#'
#' @examples
#' # Passes silently
#' assert_are_names_matching(data.frame(a = 1), match = c("a"))
#' # Errs
#' testthat::expect_error(
#'   assert_are_names_matching(data.frame(a = 1), match = c("aa"))
#' )
#' # Warns
#' assert_are_names_matching(data.frame(a = 1), match = c("aa"),
#'   severity = "warning")
#' @keywords internal
#' @export 
assert_are_names_matching <- function(.data, match, ...) {
  anchored_names <- paste0("^", names(.data), "$")
  collapsed_names <- paste(anchored_names, collapse = "|")
  assertive::assert_all_are_matching_regex(match, collapsed_names, ...)
}

# ***abundance; abundance*** ----------------------------------------------

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
  validate_dbh_dbhunit_mindbh(dbh = dbh, dbhunit = dbhunit)

  if (dbhunit == "cm") {return(pi * (dbh / 200) ^ 2)}
  pi * (dbh / 2000) ^ 2
}

#' @rdname basal_area
#' @export
ba_sum <- function(dbh, dbhunit = "mm", mindbh = 10) {
  validate_dbh_dbhunit_mindbh(dbh = dbh, dbhunit = dbhunit, mindbh = mindbh)
  
  if (!is.null(mindbh)) {
    dbh <- dbh[dbh >= mindbh]
  }
  if (length(dbh) == 0) {
    return(0)
  }
  sum(ba(dbh, dbhunit = dbhunit), na.rm = TRUE)
}

validate_dbh_dbhunit_mindbh <- function(dbh, dbhunit, mindbh = NULL) {
  assertive::assert_is_numeric(dbh)
  assertive::assert_any_are_matching_regex(dbhunit, pattern = "^mm$|^cm$")
  assertive::assert_all_are_positive(dbh, na_ignore = TRUE)
  assertive::assert_all_are_not_na(dbh, severity = "warning")
  if (!is.null(mindbh)) {
    assertive::assert_is_a_number(mindbh, severity = "warning")
    assertive::assert_all_are_positive(mindbh)
  }
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
  validate_arguments_abundance(
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

validate_arguments_abundance <- function(censdata,
                                         type = "abund",
                                         alivecode = c("A"),
                                         mindbh = NULL,
                                         dbhunit = "mm",
                                         split1 = NULL,
                                         split2 = NULL) {
  required_names <- c("dbh", "status", "agb", "date")
  censdata_names <- paste(colnames(censdata), collapse = "|")
  assertive::assert_all_are_matching_regex(required_names, censdata_names)
  assertive::assert_any_are_matching_regex(type, "abund|ba|agb")
  if (!is.null(mindbh)) {
    assertive::assert_is_numeric(mindbh)
  }
  assertive::assert_any_are_matching_regex(dbhunit, "mm|cm")

  if (!is.null(split1)) {
    assertive::assert_is_of_length(split1, nrow(censdata))
    
    vector_passed <- deparse(substitute(split1))
    assertive::assert_all_are_matching_regex(
      vector_passed, censdata_names, severity = "warning"
    )
  }
  
  if (!is.null(split2)) {
    assertive::assert_is_of_length(split2, nrow(censdata))
    
    vector_passed <- deparse(substitute(split2))
    assertive::assert_all_are_matching_regex(
      vector_passed, censdata_names, severity = "warning"
    )
  }
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

# Reject obviously incorrect data
validate_abundance_match_census_habitat <- function(censdata, habitats) {
  assert_are_names_matching(censdata, c("tag", "sp"))
  assert_are_names_matching(habitats, c("x", "y"))
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



# ***extras*** ------------------------------------------------------------

# Name of species which stems are n or above 

# Provide readable conditional stop, with friendly message
stop_if_n_is_too_high <- function(x, n) {
  assertive::assert_is_numeric(x)
  assertive::assert_is_numeric(n)
  assertive::assert_is_scalar(n)

  if (n > max(x)) {
    stop(
      "No species has so many alive stems as n = ", deparse(n), ". ",
      "Provide n <= ", deparse(max(x)), "."
    )
  }
}

#' Name of species which count is at or above a threshold.
#'
#' Name of species which count is at or above a threshold.
#'
#' @template censdata
#' @template abundances
#' @template n
#'
#' @name sp_names_by_n
#'
#' @return A character string giving the names of species which count of alive
#'   stems ([sp_alive_n()]) or total abundance per quadrat ([sp_abund_n()]) is
#'   at or above a threshold.
#'
#' @examples
#' # With census data
#' sp_alive_n(censdata = bci_mini, n = 1500)
#'
#' # With abundance data
#' abund <- abundance_match_census_habitat(bci_mini, bci_habitat)
#' sp_abund_n(abund, n = 1500)
NULL

#' @rdname sp_names_by_n
#' @export
sp_alive_n <- function(censdata, n) {
  assertive::assert_is_data.frame(censdata)
  assertive::assert_is_scalar(n)
  assertive::assert_all_are_non_negative(n)

  alive_stems <- censdata[censdata$status == "A", "sp"]
  alive_stems_count <- table(alive_stems)

  stop_if_n_is_too_high(x = alive_stems_count, n = n)

  alive_n_plus <- alive_stems_count[alive_stems_count >= n]
  names(alive_n_plus)
}

#' @rdname sp_names_by_n
#' @export
sp_abund_n <- function(abundances, n) {
  assertive::assert_is_data.frame(abundances)
  assertive::assert_is_non_empty(abundances)
  assertive::assert_is_non_empty(n)
  assertive::assert_is_scalar(n)
  assertive::assert_is_numeric(n)

  rs <- rowSums(abundances)
  stop_if_n_is_too_high(x = rs, n = n)

  names(rs[rs >= n])
}







