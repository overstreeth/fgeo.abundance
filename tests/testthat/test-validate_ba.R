context("validate_ba")

test_that("is silent with good input", {
  dbh <- c(1:10)
  dbhunit <- "cm"
  expect_silent(
    validate_ba(dbh = dbh, dbhunit = dbhunit)
  )
})

test_that("stops with wrong input", {
  dbh <- "wrong"
  dbhunit <- "cm"
  expect_error(
    validate_ba(dbh = dbh, dbhunit = dbhunit)
  )
  dbh <- c(1:10)
  dbhunit <- "cmm"
  expect_error(
    validate_ba(dbh = dbh, dbhunit = dbhunit)
  )
  dbhunit = "cm"
})

test_that("warns with wrong input", {
  dbh = c(1:10, NA)
  dbhunit = "cm"
  expect_warning(
    validate_ba(dbh = dbh, dbhunit = dbhunit)
  )
})
