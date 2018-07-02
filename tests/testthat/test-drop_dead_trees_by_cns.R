context("drop_dead_trees_by_cns")

test_that("outputs expected warning and names", {
  
  # First pick the data you want
  vft <- data.frame(
    Status = "alive trees", 
    PlotCensusNumber = 1,
    TreeID = 1,
    CensusID = 1
  )
  expect_message(
    suppressWarnings(out <- drop_dead_trees_by_cns(vft)), 
    "Fixing status automatically"
  )
  nms <- c("Status", "PlotCensusNumber", "TreeID", "CensusID", "status_tree")
  expect_named(out, nms)
})

test_that("outputs expected message and names", {
  # First pick the data you want
  vft <- data.frame(
    Status = c("dead", "alive"), 
    PlotCensusNumber = 1:2,
    TreeID = 1:2,
    CensusID = 1:2
  )
  expect_message(
    out <- filter_tree_status_by_census(vft, "dead", TRUE, c("dead", "alive")),
    "Dropping"
  )
  nms <- c("Status", "PlotCensusNumber", "TreeID", "CensusID", "status_tree")
  expect_named(out, nms)
  
})

