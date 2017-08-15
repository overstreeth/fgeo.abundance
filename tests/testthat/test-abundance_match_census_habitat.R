context("abundance_match_census_habitat")

test_that("value is a data frame with subset equal to stored reference", {
  ref <- abundanceperquad(
    bci_mini, plotdim = c(1000, 500), gridsize = 20
  )$abund
  actual <- abundance_match_census_habitat(bci_mini, bci_habitat)
  expect_equal(ref, actual)
})
