context("byyr2")

test_that("byyr2 outputs the same as byyr during refactoring", {
  
  library(fgeo.abundance)
  library(fgeo.tool)
  library(fgeo.base)
  
  vft <- fgeo.data::luquillo_vft_4quad
  vft_plot <- pick_plotname(vft, "luquillo")
  one_stem_per_treeid <- pick_largest_hom_dbh(vft_plot)
  # This filters on dbh, so it'll remove missing dbh and therefore dead trees
  stems <- pick_trees(one_stem_per_treeid)
  
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
