context("add_basal_area")

stem <- tibble::tibble(
  stemID = 1:4,
  quadrat = paste0("000", rep(1:2, each = 2)),

  sp = c("sp1", "sp2", "sp1", "sp1"),
  dbh = c(1, 1, 2, 2)
)

test_that("returns similar to basal_area(group_by()) but all rows", {
  expect_equal(
    unique(basal_area(group_by(stem, quadrat))$basal_area),
    unique(add_basal_area(stem, quadrat)$basal_area)
  )
  expect_equal(nrow(basal_area(group_by(stem, quadrat))), 2)
  expect_equal(nrow(add_basal_area(stem, quadrat)), 4)
})

test_that("returns dataframe with expected structure", {
  expect_is(add_basal_area(stem, quadrat), "data.frame")
  expect_named(add_basal_area(stem, quadrat), c(names(stem), "basal_area"))
})

test_that("deals with grouping", {
  # outputs grouping as it was in input
  # ungrouped -> ungrouped
  expect_false(is_grouped_df(add_basal_area(stem)))
  # grouped -> grouped
  expect_true(is_grouped_df(add_basal_area(group_by(stem, quadrat))))
})
