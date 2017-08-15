context("sp_alive_n")

test_that("sp_alive_n stops if n is too high", {
  expect_error(
    sp_alive_n(bci_mini, n = 40000)
  )
})

test_that("The error message matches the expected string:", {
  expected_string <- "No species has so many alive stems as n ="
  expect_error(
    sp_alive_n(censdata = bci_mini, n = 40000),
    expected_string
  )
})

test_that("value is a non-empty character string", {
  value <- sp_alive_n(bci_mini, 1000)
  expect_true(assertive::is_non_empty(value))
  expect_type(value, "character")
})

test_that("sp_alive_n stops with invalid data", {
  # not a dataframe
  expect_error(sp_alive_n(as.matrix(bci_mini), n = 1000))
  # not a scalar
  expect_error(sp_alive_n(bci_mini, n = c(1000, 1500)))
  # not non-negative
  expect_error(sp_alive_n(bci_mini, n = -1000))
  expect_error(sp_alive_n(bci_mini, n = NA_integer_))
  expect_error(sp_alive_n(bci_mini, n = NA_real_))
})
