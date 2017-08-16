context("validate_sp_abund_n")

# Warnings here are intentional and can be disregarded in this context.
expect_warning(
  abund <- abundance_match_census_habitat(bci_mini, bci_habitat)
)

test_that("errs with wrong input", {
  # Check is OK with correct input
  expect_silent(
    validate_sp_abund_n(abund, 1000)
  )
  expect_error(
    validate_sp_abund_n(abundances = as.matrix(abund), 1000)
  )
  expect_error(
    validate_sp_abund_n(abund, n = numeric(0))
  )
  expect_error(
    validate_sp_abund_n(abund, n = 1:3)
  )
  expect_error(
    validate_sp_abund_n(abund, n = as.factor(1000))
  )
})

