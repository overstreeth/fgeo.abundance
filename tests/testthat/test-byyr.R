context("byyr")

library(tibble)
library(dplyr)
library(fgeo.base)

vft <- readr::read_csv(test_path("data-byyr_toy_vft.csv"))

test_that("basal_area_byyr and abundance_byyr fail with informative errors", {
  expect_error(abundance_byyr(1), "data.frame.*is not TRUE")
  expect_error(abundance_byyr(), "is missing")
  
  msg <-  "All expressions.*must refer to `dbh`"
  
  expect_error(abundance_byyr(vft, exactdate > 0), msg)
  expect_silent(abundance_byyr(vft, DBH > 0))
  expect_silent(abundance_byyr(vft, exactdate > 0, dbh > 0))
  
  expect_error(abundance_byyr(vft), msg)
  expect_error(abundance_byyr(vft, invalid > 0), msg)
  
  expect_error(basal_area_byyr(1), "data.frame.*is not TRUE")
  expect_error(basal_area_byyr(), "is missing")
  
  expect_silent(basal_area_byyr(vft, DBH > 0))
  expect_silent(basal_area_byyr(vft, exactdate > 0, dbh > 0))
  
  expect_error(basal_area_byyr(vft), msg)
  expect_error(basal_area_byyr(vft, exactdate > 0), msg)
  expect_error(basal_area_byyr(vft, invalid > 0), msg)
})

describe("basal_area_byyr and abundance_byyr work with different datasets", {
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
    out_basal <- byyr(vft, plot, basal_area_byyr)
    expect_dataframe(out_basal)
    expect_named_year(out_basal, "2005")
    
    out_abund <- byyr(vft, plot, abundance_byyr)
    expect_dataframe(out_abund)
    expect_named_year(out_abund, "2005")
  })
  
  it("works with data from from Bukit Timah", {
    skip_if_not_installed("bukittimah")
    bukit <- bukittimah::ViewFullTable_bukit
    plot <- "Bukit Timah Big Trees"
    
    out_basal <-  byyr(bukit, plot, basal_area_byyr)
    expect_dataframe(out_basal)
    expect_named_year(out_basal, "2006")
    
    out_abund <- byyr(bukit, plot, abundance_byyr)
    expect_dataframe(out_basal)
    expect_named_year(out_basal, "2006")
  })
  
  it("works with data from from Ngel Niaky", {
    skip_if_not_installed("ngel")
    
    ngel <- ngel::ViewFullTable_ngel
    plot <- "ngelnyaki"
    out_basal <-  byyr(ngel, plot, basal_area_byyr)
    expect_dataframe(out_basal)
    expect_named_year(out_basal, "2015")
    
    out_abund <- byyr(ngel, plot, abundance_byyr)
    expect_dataframe(out_basal)
    expect_named_year(out_basal, "2015")
  })
})

describe("abundance_byyr and basa_area_byyr return expected output", {
  tiny <- tibble::tibble(
    Tag = c("0001", "0001", "0002", "0002", "0003", "0003"),
    TreeID = c("0001", "0001", "0002", "0002", "0003", "0003"),
    PlotName = c("p", "p", "p", "p", "p", "p"),
    Status = c("alive", "alive", "alive", "alive", "alive", "alive"),
    DBH = c(1L),
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
  
  it("outputs basal area multiplied by the abundance", {
    abund <- abundance_byyr(tiny, dbh > 0)
    basal <- basal_area_byyr(tiny, dbh > 0)
    expect_equal(basal$`2000`, basal_area(1) * abund$`2000`)
    expect_equal(basal$`2001`, basal_area(1) * abund$`2001`)
    
    tiny2 <- mutate(tiny, DBH = 10)
    basal <- basal_area_byyr(tiny2, dbh > 0)
    expect_equal(basal$`2000`, basal_area(10) * abund$`2000`)
    expect_equal(basal$`2001`, basal_area(10) * abund$`2001`)
  })
  
  it("outputs equal to known output", {
    out1 <- suppressWarnings(suppressMessages(abundance_byyr(tiny, dbh > 0)))
    expect_equal(out1$`2000`, c(1, 2))
    expect_equal(out1$`2001`, c(1, 2))
    
    tiny2 <- tiny[1:4, ]
    out2 <- suppressWarnings(suppressMessages(abundance_byyr(tiny2, dbh > 0)))
    expect_equal(out2$`2000`, c(1, 1))
    expect_equal(out2$`2001`, c(1, 1))
    
    tiny3 <- tiny[c(1, 3, 4), ]
    out3 <- suppressWarnings(suppressMessages(abundance_byyr(tiny3, dbh > 0)))
    expect_equal(out3$`2000`, c(1, 1))
    expect_equal(out3$`2001`, c(0, 1))
  })
})

describe("abundance_byyr", {
  skip_if_not_installed("readr")
  
  it("lowercases dbh and only dbh from the expression passed to ...", {
    
    expect_silent(
      out <- abundance_byyr(vft, dbh >= min(vft$DBH, na.rm = TRUE))
    )
    expect_equal(out, abundance_byyr(vft, dbh > 0))
  })
  
  it("is sensitive to DBH, so outputs none date-column if dbh is too big ", {
    
    too_big <- max(vft$DBH, na.rm = TRUE) + 1
    out <- abundance_byyr(vft, dbh > !! too_big)
    expect_named(rlang::set_names(out, tolower), c("species", "family"))
    expect_is(out, "tbl_df")
    
    # Upper case DBH
    expect_equal(out, abundance_byyr(vft, DBH > !! too_big))
  })
  
  it("outputs as expected", {
    # All trees are of the same species. There are two trees, each with two
    # stems. In census 1, the count of alive trees should be 2 because both
    # trees are alive, but note that one stem is dead (StemID = 1.2). In census
    # 2 the count of alive trees should be 1:
    #   * One tree is alive (TreeID = 1) although one stem is gone 
    #     (StemID = 1.2);
    #   * One tree is dead (TreeID = 2) because both its stems are dead.
    
    
    out <- abundance_byyr(vft, dbh > 0)
    expect_is(out, "tbl_df")
    expect_named(set_names(out, tolower), c("species", "family", "2001", "2002"))
    expect_equal(out$`2001`, 2)
    expect_equal(out$`2002`, 1)
  })
  
  it("fails if parsed dates are all missing", {
    
    bad <- mutate(vft[1, ], ExactDate = NA)
    msg <- "Can't parse `exactdates`"
    expect_error(abundance_byyr(bad, dbh > 0), msg)
    
    # Wrong format: Expecting yyy-mm-dd, so parsing results in NA
    bad <- mutate(vft[1, ], ExactDate = as.character("1/1/2001"))
    expect_error(
      expect_warning(abundance_byyr(bad, dbh > 0)), "Can't parse `exactdates`"
    )
  })
  
  it("warns if parsed dates are not from 1980 to present", {
    
    early <- mutate(vft[1, ], ExactDate = "1970-01-01")
    msg <- "Dates should be"
    expect_warning(abundance_byyr(early, dbh > 0), msg)
    
    late <- mutate(vft[1, ], ExactDate = lubridate::today() + 1)
    msg <- "Dates should be"
    expect_warning(abundance_byyr(late, dbh > 0), msg)
    
    good <- mutate(vft[1, ], ExactDate = lubridate::today())
    expect_silent(abundance_byyr(good, dbh > 0))
  })
})

describe("*byyr()", {
  it("makes no difference if status is picked before *byyr()", {
    skip_if_not_installed("readr")
    raw <- abundance_byyr(vft, dbh > 0)
    picked <- abundance_byyr(filter(vft, Status == "alive"), dbh > 0)
    expect_equal(picked, raw)
  })
})

describe("basal_area_byyr()", {
  it("sums basal area of all stems which dbh is in the chosen range", {
    # DRY helper
    actual_equals_expected <- function(vft) {
      vft <- mutate(vft, Status = "alive", DBH = 1)
      expected <- basal_area(1) * nrow(vft)
      actual <- basal_area_byyr(vft, dbh >= 1)$`2001`
      expect_equal(actual, expected)
    }
    
    # One census; one tree with two stems.
    vft_simple <- mutate(vft, Status = "alive", DBH = 1)
    
    vft_c1_t1_s2 <- filter(vft_simple, CensusID == 1, TreeID == 1)
    expected <- basal_area(1) * nrow(vft_c1_t1_s2)
    actual <- basal_area_byyr(vft_c1_t1_s2, dbh >= 1)$`2001`
    expect_equal(actual, expected)
    
    # One census; two trees, each with two stems.
    vft_c1_t2_s4 <- filter(vft_simple, CensusID == 1)
    expected <- basal_area(1) * nrow(vft_c1_t2_s4)
    actual <- basal_area_byyr(vft_c1_t2_s4, dbh >= 1)$`2001`
    expect_equal(actual, expected)
    
    # Two censuses, with one tree in each
    vft_c2_t1_s4 <- filter(vft_simple, TreeID == 1)
    expected_total <- basal_area(1) * nrow(vft_c2_t1_s4)
    actual_2001 <- basal_area_byyr(vft_c2_t1_s4, dbh >= 1)$`2001`
    actual_2002 <- basal_area_byyr(vft_c2_t1_s4, dbh >= 1)$`2002`
    expect_equal(actual_2001 + actual_2002, expected_total)
  })
})

