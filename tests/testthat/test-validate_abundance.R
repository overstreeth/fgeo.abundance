
context("validate_abundance")

library(dplyr)
cns5_mini <- sample_n(bci::bci12full5, 100)

test_that("throws ERROR with wrong input", {
  expect_error(
    validate_abundance(censdata = data.frame(a = 1:3))
  )
  expect_error(
    validate_abundance(censdata = cns5_mini, type = "wrong")
  )
  expect_error(
    validate_abundance(censdata = cns5_mini, mindbh = "wrong")
  )
  expect_error(
    validate_abundance(censdata = cns5_mini, dbhunit = "wrong")
  )
  wrong_length <- rep(1:3, nrow(cns5_mini))
  expect_error(
    validate_abundance(censdata = cns5_mini, split1 = wrong_length)
  )
  expect_error(
    validate_abundance(censdata = cns5_mini, split2 = wrong_length)
  )
})

test_that("is SILENT with good arguments", {
  expect_silent(
    validate_abundance(censdata = as.matrix(cns5_mini))
  )
  expect_silent(
    validate_abundance(
      censdata = as.data.frame(cns5_mini), type = "agb"
    )
  )
  expect_silent(
    validate_abundance(censdata = cns5_mini, split1 = cns5_mini$sp)
  )
  MeasureID <- rep(1, nrow(cns5_mini))
  expect_silent(
    validate_abundance(censdata = cns5_mini, split1 = MeasureID)
  )
  MeasureID <- rep(1, nrow(cns5_mini))
  expect_silent(
    validate_abundance(censdata = cns5_mini, split2 = MeasureID)
  )
})

test_that("WARNS with suspicious arguments", {
    measureid <- rep(1, nrow(cns5_mini))
    expect_warning(
      validate_abundance(censdata = cns5_mini, split1 = measureid)
    )
    measureid <- rep(1, nrow(cns5_mini))
    expect_warning(
      validate_abundance(censdata = cns5_mini, split2 = measureid)
    )
})

