library(testthat)
context("assert_are_names_matching")

test_that("matches names in censdata", {
  expect_silent(
    assert_are_names_matching(
      bci_mini, match = c("tag", "sp"), severity = "warning"
    )
  )
  expect_warning(
    assert_are_names_matching(
      bci_mini, match = c("tag", "spp"), severity = "warning"
    )
  )
  expect_error(
    assert_are_names_matching(
      bci_mini, match = c("Tag"), severity = "stop"
    )
  )
  expect_error(assert_are_names_matching(bci_mini, match = c("Tag")))
})

test_that("works with column names of matrix or names of dataframe", {
  expect_silent(
    assert_are_names_matching(data.frame(a = 1), "a")
  )
  expect_silent(
    assert_are_names_matching(as.matrix(data.frame(a = 1)), "a")
  )
})

test_that("stops with wrong input", {
  expect_error(
    assert_are_names_matching(.data = "wrong", "a")
  )
  expect_error(
    assert_are_names_matching(.data = data.frame(a = 1), as.factor("a"))
  )
})
