context("basal_area")

# `ba()` ------------------------------------------------------------------

test_that("`ba()` outputs the same as original `ctfs::ba()`", {
  dbh <- c(1, 23, 43)
  expect_equal(ba(dbh = dbh), ctfs::ba(dbh = dbh))
})
test_that("in `ba()` dbhunit defaults to milimiters", {
  dbh <- c(1, 23, 43)
  expect_equal(forestr::ba(dbh = dbh), ba(dbh = dbh, dbhunit = "mm"))
  expect_false(identical(ba(dbh = dbh), ba(dbh = dbh, dbhunit = "cm")))
})
test_that("`ba()` errs with wrong dbhunit", {
  expect_error(ba(dbh, "wrong"))
})
test_that("`ba()` errs with wrong dbh", {
  expect_error(ba(c(0, 1, 2)))
  expect_warning(ba(dbh = c(NA, 1, 2, 3)))
})



# `ba_sum()` ---------------------------------------------------------------

test_that("`ba_sum()` outputs the same as original `ctfs::basum()`", {
  dbh <- c(1, 23, 43)
  expect_equal(
    forestr::ba_sum(dbh = dbh, mindbh = 10), 
    ctfs::basum(dbh = dbh, mindbh = 10)
  )
  expect_equal(
    forestr::ba_sum(dbh = dbh, mindbh = 23, dbhunit = "cm"), 
    ctfs::basum(dbh = dbh, mindbh = 23, dbhunit = "cm")
  )
})
test_that("in `ba_sum()` dbhunit defaults to milimiters", {
  dbh <- c(1, 23, 43)
  expect_equal(
    ba_sum(dbh = dbh, mindbh = 10), 
    ba_sum(dbh = dbh, mindbh = 10, dbhunit = "mm")
  )
  expect_false(
    identical(
      ba_sum(dbh = dbh, mindbh = 10), 
      ba_sum(dbh = dbh, mindbh = 10, dbhunit = "cm")
    )
  )
})
test_that("`ba_sum()` errs with wrong arguments", {
  expect_error(ba_sum("wrong", mindbh = 10, dbhunit = "cm"))
  expect_error(ba_sum(dbh, mindbh = 10, dbhunit = "wrong"))
  expect_error(ba_sum(dbh, mindbh = "wrong"))
  expect_error(ba_sum(0:2))
})
test_that("`ba_sum()` warns with NAs in dbh", {
  expect_warning(ba_sum(dbh = c(NA, 1, 2, 3)))
})
test_that("`ba_sum()` returns 0 if length of dbh is 0", {
  expect_equal(ba_sum(dbh = numeric(0)), 0)
})
