context("sp_abund_n")

test_that("value is character string", {
  # Warnings here are ok. Learm more at test-abundance_match_census_habitat.R
  expect_warning(
    abund <- abundance_match_census_habitat(bci_mini, bci_habitat)
  )
  value <- sp_abund_n(abund, 1000)
  expect_type(value, "character")
})

test_that("with bci_mini, returns c('hybapr', 'faraoc')", {
  expect_warning(
    abund <- abundance_match_census_habitat(bci_mini, bci_habitat)
  )
  value <- sp_abund_n(abund, 1000)
  expect_equal(value, c("hybapr", "faraoc"))
})
