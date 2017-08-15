context("stop_if_n_is_too_high")

test_that("stops only if n is too high", {
  x <- c(1, 5, 10)
  expect_error(stop_if_n_is_too_high(x, 20))
  expect_silent(stop_if_n_is_too_high(x, 5))
})

test_that("error if x is not numeric", {
  x <- "hi"
  expect_error(stop_if_n_is_too_high(x, 20))
})
