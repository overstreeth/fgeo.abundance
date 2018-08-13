context("byyr2")

describe("abundance_byyr2", {
  skip_if_not_installed("readr")
  
  vft <- readr::read_csv(test_path("data-byyr_toy_vft.csv"))
  it("lowercases dbh and only dbh from the expression passed to ...", {
    expect_silent(
      out <- abundance_byyr2(vft, dbh >= min(vft$DBH, na.rm = TRUE))
    )
    expect_equal(out, abundance_byyr2(vft, dbh > 0))
  })
  
  
  
  it("is sensitive to DBH, so outputs none date-column if dbh is too big ", {
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
    expect_error(abundance_byyr2(bad), msg)
    
    # Wrong format: Expecting yyy-mm-dd, so parsing results in NA
    bad <- mutate(vft[1, ], ExactDate = as.character("1/1/2001"))
    msg <- "Can't parse `exactdates`"
    expect_warning(expect_error(abundance_byyr2(bad), msg))
  })
  
  it("warns if parsed dates are not from 1980 to present", {
    vft <- readr::read_csv(test_path("data-byyr_toy_vft.csv"))
    early <- mutate(vft[1, ], ExactDate = "1970-01-01")
    msg <- "Dates should be"
    expect_warning(abundance_byyr2(early), msg)
    
    late <- mutate(vft[1, ], ExactDate = lubridate::today() + 1)
    msg <- "Dates should be"
    expect_warning(abundance_byyr2(late), msg)
    
    good <- mutate(vft[1, ], ExactDate = lubridate::today())
    expect_silent(abundance_byyr2(good))
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
