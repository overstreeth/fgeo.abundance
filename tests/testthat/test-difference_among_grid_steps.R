context("difference_among_grid_steps")

bci_habitat <- bci::bci_habitat
test_that("value is a non negative scalar", {
  habx <- bci_habitat$x
  haby <- bci_habitat$y
  value <- difference_among_grid_steps(habx)
  expect_true(assertive::is_scalar(value))
  expect_silent(assertive::assert_all_are_non_negative(value))
})
