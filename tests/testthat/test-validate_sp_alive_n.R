context("validate_sp_alive_n")



test_that("validate_sp_alive_n stops with invalid data", {
  # not a dataframe
  expect_error(validate_sp_alive_n(as.matrix(bci_mini), n = 1000))
  # not a scalar
  expect_error(validate_sp_alive_n(bci_mini, n = c(1000, 1500)))
  # not non-negative
  expect_error(validate_sp_alive_n(bci_mini, n = -1000))
  expect_error(validate_sp_alive_n(bci_mini, n = NA_integer_))
  expect_error(validate_sp_alive_n(bci_mini, n = NA_real_))
})
