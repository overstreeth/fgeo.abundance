context("byyr2")

test_that("byyr2 outputs the same as byyr during refactoring", {
  
  library(fgeo.abundance)
  library(fgeo.tool)
  library(fgeo.base)
  
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
  stems <- pick_trees(vft_plot)
  out_abund <- abundance_byyr2(stems)
  
  expect_known_output(
    as.data.frame(out_abund), 
    "ref-composed-abundance-byyr", print = TRUE, update = FALSE
  )
  
  out_ba <- basal_area_byyr2(stems)
  expect_known_output(
    as.data.frame(out_ba), 
    "ref-composed-basal-area-byyr", print = TRUE, update = FALSE
  )
})
