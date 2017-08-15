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
