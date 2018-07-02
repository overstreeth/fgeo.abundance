context("drop_dead_trees_by_cns")

# First pick the data you want
vft <- data.frame(
  Status = "alive trees", 
  PlotCensusNumber = 1,
  TreeID = 1,
  CensusID = 1
)

test_that("outputs expected warning", {
  expect_message(
    suppressWarnings(out <- drop_dead_trees_by_cns(vft)), 
    "Fixing status automatically"
  )
  nms <- c("Status", "PlotCensusNumber", "TreeID", "CensusID", "status_tree")
  expect_named(out, nms)
})
