context("byyr2")

library(dplyr)
library(fgeo.base)

describe("basal_area_byyr2 and abundance_byyr2", {
  # DRY helpers
  byyr <- function(.data, plot, .f) {
    luq <- suppressMessages(pick_plotname(.data, plot))
    # Fix seed to reproduce random-sampling
    out <- withr::with_seed(123, sample_n(luq, 10))
    .f(out, dbh > 0)
  }
  
  expect_dataframe <- function(x) expect_is(x, "data.frame")
  
  expect_named_year <- function(x, year) {
    # Test only 1 year-column to avoid errors as data grows
    expect_named(x[1:3], c("species", "Family", year))
  }
  
  it("works with data from from bci", {
    skip_if_not_installed("bciex")
    # Unexpected name
    vft <- bciex::bci12vft_mini %>% rename(PlotName = Plot)
    plot <- "bci"
    out_basal <- byyr(vft, plot, basal_area_byyr2)
    expect_dataframe(out_basal)
    expect_named_year(out_basal, "2005")
    
    out_abund <- byyr(vft, plot, abundance_byyr2)
    expect_dataframe(out_abund)
    expect_named_year(out_abund, "2005")
  })
  
  it("works with data from from Bukit Timah", {
    skip_if_not_installed("bukittimah")
    vft <- bukittimah::ViewFullTable_bukit
    plot <- "Bukit Timah Big Trees"
    
    out_basal <-  byyr(vft, plot, basal_area_byyr2)
    expect_dataframe(out_basal)
    expect_named_year(out_basal, "2006")
    
    out_abund <- byyr(vft, plot, abundance_byyr2)
    expect_dataframe(out_basal)
    expect_named_year(out_basal, "2006")
  })
  
  it("works with data from from Ngel Niaky", {
    skip_if_not_installed("ngel")
    
    vft <- ngel::ViewFullTable_ngel
    plot <- "ngelnyaki"
    out_basal <-  byyr(vft, plot, basal_area_byyr2)
    expect_dataframe(out_basal)
    expect_named_year(out_basal, "2015")
    
    out_abund <- byyr(vft, plot, abundance_byyr2)
    expect_dataframe(out_basal)
    expect_named_year(out_basal, "2015")
  })
})



describe("abundance_byyr2", {
  skip_if_not_installed("readr")
  
  it("outputs equal to known output", {
    tiny <- tibble::tibble(
      Tag = c("0001", "0001", "0002", "0002", "0003", "0003"),
      TreeID = c("0001", "0001", "0002", "0002", "0003", "0003"),
      PlotName = c("p", "p", "p", "p", "p", "p"),
      Status = c("alive", "alive", "alive", "alive", "alive", "alive"),
      DBH = c(1L, 5L, 2L, 6L, 2L, 6L),
      ExactDate = c(
        "2000-01-01", "2001-01-01", "2000-01-01", "2001-01-01", "2000-01-01",
        "2001-01-01"
      ),
      PlotCensusNumber = c(1L, 2L, 1L, 2L, 1L, 2L),
      CensusID = c(1L, 2L, 1L, 2L, 1L, 2L),
      Genus = c("A", "A", "B", "B", "B", "B"),
      SpeciesName = c("a", "a", "b", "b", "b", "b"),
      Family = c("f", "f", "f", "f", "f", "f"),
      StemID = 1,
      HOM = 130
    )
    out1 <- suppressWarnings(suppressMessages(abundance_byyr2(tiny, dbh > 0)))
    expect_equal(out1$`2000`, c(1, 2))
    expect_equal(out1$`2001`, c(1, 2))
    
    tiny2 <- tiny[1:4, ]
    out2 <- suppressWarnings(suppressMessages(abundance_byyr2(tiny2, dbh > 0)))
    expect_equal(out2$`2000`, c(1, 1))
    expect_equal(out2$`2001`, c(1, 1))
    
    tiny3 <- tiny[c(1, 3, 4), ]
    out3 <- suppressWarnings(suppressMessages(abundance_byyr2(tiny3, dbh > 0)))
    expect_equal(out3$`2000`, c(1, 1))
    expect_equal(out3$`2001`, c(0, 1))
  })
  
  it("fails with common inputs with informative error", {
    expect_error(abundance_byyr2(1), "data.frame.*is not TRUE")
    expect_error(abundance_byyr2(), "is missing")
    
    vft <- readr::read_csv(test_path("data-byyr_toy_vft.csv"))
    expect_silent(abundance_byyr2(vft, DBH > 0))
    expect_warning(abundance_byyr2(vft, exactdate > 0), "forget.*dbh range?")
    expect_silent(abundance_byyr2(vft, exactdate > 0, dbh > 0))
    expect_warning(abundance_byyr2(vft), "forget to pick a specific dbh range?")
    expect_error(
      expect_warning(abundance_byyr2(vft, invalid > 0)), 
      "object.*not found"
    )
  })
  
  it("lowercases dbh and only dbh from the expression passed to ...", {
    vft <- readr::read_csv(test_path("data-byyr_toy_vft.csv"))
    expect_silent(
      out <- abundance_byyr2(vft, dbh >= min(vft$DBH, na.rm = TRUE))
    )
    expect_equal(out, abundance_byyr2(vft, dbh > 0))
  })
  
  it("is sensitive to DBH, so outputs none date-column if dbh is too big ", {
    vft <- readr::read_csv(test_path("data-byyr_toy_vft.csv"))
    too_big <- max(vft$DBH, na.rm = TRUE) + 1
    out <- abundance_byyr2(vft, dbh > !! too_big)
    expect_named(rlang::set_names(out, tolower), c("species", "family"))
    expect_is(out, "tbl_df")

    # Upper case DBH
    expect_equal(out, abundance_byyr2(vft, DBH > !! too_big))
  })
  
  it("outputs as expected", {
    # All trees are of the same species. There are two trees, each with two
    # stems. In census 1, the count of alive trees should be 2 because both
    # trees are alive, but note that one stem is dead (StemID = 1.2). In census
    # 2 the count of alive trees should be 1:
    #   * One tree is alive (TreeID = 1) although one stem is gone 
    #     (StemID = 1.2);
    #   * One tree is dead (TreeID = 2) because both its stems are dead.
    vft <- readr::read_csv(test_path("data-byyr_toy_vft.csv"))
    
    out <- abundance_byyr2(vft, dbh > 0)
    expect_is(out, "tbl_df")
    expect_named(set_names(out, tolower), c("species", "family", "2001", "2002"))
    expect_equal(out$`2001`, 2)
    expect_equal(out$`2002`, 1)
  })
  
  it("fails if parsed dates are all missing", {
    vft <- readr::read_csv(test_path("data-byyr_toy_vft.csv"))
    bad <- mutate(vft[1, ], ExactDate = NA)
    msg <- "Can't parse `exactdates`"
    expect_error(abundance_byyr2(bad, dbh > 0), msg)
    
    # Wrong format: Expecting yyy-mm-dd, so parsing results in NA
    bad <- mutate(vft[1, ], ExactDate = as.character("1/1/2001"))
    expect_error(
      expect_warning(abundance_byyr2(bad, dbh > 0)), "Can't parse `exactdates`"
    )
  })
  
  it("warns if parsed dates are not from 1980 to present", {
    vft <- readr::read_csv(test_path("data-byyr_toy_vft.csv"))
    early <- mutate(vft[1, ], ExactDate = "1970-01-01")
    msg <- "Dates should be"
    expect_warning(abundance_byyr2(early, dbh > 0), msg)
    
    late <- mutate(vft[1, ], ExactDate = lubridate::today() + 1)
    msg <- "Dates should be"
    expect_warning(abundance_byyr2(late, dbh > 0), msg)
    
    good <- mutate(vft[1, ], ExactDate = lubridate::today())
    expect_silent(abundance_byyr2(good, dbh > 0))
  })
})








# TODO: Remove after integrating byyr and byyr2 ---------------------------

test_that("byyr2 outputs the same as byyr during refactoring", {
  vft <- fgeo.data::luquillo_vft_4quad
  
  # Recommended preparation
  
  # This should make no difference because later pick_trees() excludes missing
  # dbh -- which also removes any stem which status isn't alive. Yet, it's good
  # practice to include this line because it is better to be explicit rather 
  # than implicit.
  vft <- pick_status(vft, "alive")
  vft_plot <- pick_plotname(vft, "luquillo")
  
  # Start of wrapper

  # This filters on dbh, so it removes missing dbh and therefore all non-alive
  # stems (alive stems are the only ones which a non-missing value of dbh)
  out_abund <- abundance_byyr2(vft_plot, dbh >= 100)
  
  expect_known_output(
    as.data.frame(out_abund), 
    "ref-composed-abundance-byyr", print = TRUE, update = FALSE
  )
  
  out_ba <- basal_area_byyr2(vft_plot, dbh >= 100)
  expect_known_output(
    as.data.frame(out_ba), 
    "ref-composed-basal-area-byyr", print = TRUE, update = FALSE
  )
})
