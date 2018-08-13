context("byyr2")

library(rlang)
library(fgeo.abundance)
library(fgeo.tool)
library(fgeo.base)

test_that("outputs as expected", {
  skip_if_not_installed("fgeo.tool")
  skip_if_not_installed("readr")
  vft <- readr::read_csv(test_path("data-byyr_toy_vft.csv"))
  
  # All trees are of the same species. There are two trees, each with two stems.
  # In census 1, the count of alive trees should be 2 because both trees are
  # alive, but note that one stem is dead (StemID = 1.2). In census 2 the count
  # of alive trees should be 1:
  #   * One tree is alive (TreeID = 1) although one stem is gone (StemID = 1.2);
  #   * One tree is dead (TreeID = 2) because both its stems are dead.
  
  # Collapse treeid
  vft <- pick_largest_hom_dbh(vft)
  
  out <- abundance_byyr(vft)
  # FIXME: Activate this test
  # out <- abundance_byyr2(vft, dbh > 0)
  expect_named(set_names(out, tolower), c("species", "family", "2001", "2002"))
  expect_equal(out$`2001`, 2)
  expect_equal(out$`2002`, 1)
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
