context("validate_dbh_dbhunit_mindbh")

test_that("is silent with good input", {
  dbh = c(1:10)
  dbhunit = "cm"
  mindbh = 20
  expect_silent(
    validate_dbh_dbhunit_mindbh(dbh = dbh, dbhunit = dbhunit, mindbh = mindbh)
  )
  expect_silent(
    validate_dbh_dbhunit_mindbh(dbh = dbh, dbhunit = dbhunit)
  )
})
test_that("stops with wrong input", {
  dbh = 1:10
  dbhunit = "cm"
  mindbh = 20
  
  dbh = "wrong"
  expect_error(
    validate_dbh_dbhunit_mindbh(dbh = dbh, dbhunit = dbhunit, mindbh = mindbh)
  )
  dbh = c(-1:10)
  expect_error(
    validate_dbh_dbhunit_mindbh(dbh = dbh, dbhunit = dbhunit, mindbh = mindbh)
  )
  dbh = c(1:10)
  dbhunit = "cmm"
  expect_error(
    validate_dbh_dbhunit_mindbh(dbh = dbh, dbhunit = dbhunit, mindbh = mindbh)
  )
  dbhunit = "cm"
  mindbh = -10
  expect_error(
    validate_dbh_dbhunit_mindbh(dbh = dbh, dbhunit = dbhunit, mindbh = mindbh)
  )
})

test_that("warns with wrong input", {
  dbh = 1:10
  dbhunit = "cm"
  mindbh = 20
  
  dbh = c(1:10, NA)
  expect_warning(
    validate_dbh_dbhunit_mindbh(dbh = dbh, dbhunit = dbhunit, mindbh = mindbh)
  )
  dbh = c(1:10)
  mindbh = "wrong"
  expect_error(
    expect_warning(
      validate_dbh_dbhunit_mindbh(dbh = dbh, dbhunit = dbhunit, mindbh = mindbh)
    )
  )
})








  
  
