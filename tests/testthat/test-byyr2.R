context("byyr2")

describe("abundance_byyr2", {
  skip_if_not_installed("readr")
  
  
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
