context("abundance_match_census_habitat")

test_that("value is a data frame with subset equal to stored reference", {
  value <- abundance_match_census_habitat(bci_mini, bci_habitat)
  expect_is(value, "data.frame")
  actual <- value[1:15, 1:15]
  expect_equal_to_reference(
    actual,
    "ref_abundance_match_census_habitat.rds")
})
