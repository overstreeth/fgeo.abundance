context("fill_na_with_single_unique")

test_that("outputs is as expected", {
  expect_error(fill_na_with_single_unique(c(1, 2, NA)), "must be of length 1")
  expect_equal(fill_na_with_single_unique(c(1, NA)), c(1, 1))
  expect_equal(fill_na_with_single_unique(c(1, 1, NA)), c(1, 1, 1))
})
