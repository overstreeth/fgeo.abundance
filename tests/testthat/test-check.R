context("test-check.R")

census1 <- forestr::bci12t6mini
census2 <- forestr::bci12t7mini

test_that("check_if_all_dbh_is_na is silent or vocal as expected", {
  
  split1 = NULL
  expect_silent(
    check_if_all_dbh_is_na(cns1 = census1, cns2 = census2, split = split1)
  )

  split1 = census1$quadrat
  expect_warning(
    check_if_all_dbh_is_na(cns1 = census1, cns2 = census2, split = split1)
  )
})

context("test-check_crucial_names.R")

test_that("returns true/false if data has/has't names", {
  cns <- data.frame(a = 1, b = 1, c = 1)
  expect_silent(check_crucial_names(cns = cns, nms = c("a", "b")))
  expect_error(check_crucial_names(cns = cns, nms = c("a", "z")))
})


context("test-check_crucial_names_and_if_dbh_is_na.R")

crucial_nms <- c("dbh", "pom", "status", "date")
split1 <- census1$sp
split2 <- census1$quadrat

test_that("warns with groups which dbh is full of NA", {
  # With no splitting variable (1 and 2 are NULL) these data passes silently.
  expect_silent(
    check_crucial_names_and_if_dbh_is_na(
      census1, census2, crucial_nms, NULL, NULL
    )
  )
  # some groups in split1 and split2 have dbh full of NA
  expect_warning(
    check_crucial_names_and_if_dbh_is_na(
      census1, census2, crucial_nms, split1, split2
    )
  )
})

test_that("errs with bad names", {
  # With wrong crucial name, expect error
  census1 <- rename(census1, DBH = dbh)
  census2 <- rename(census2, DBH = dbh)
  expect_error(
    check_crucial_names_and_if_dbh_is_na(
      census1, census2, crucial_nms, NULL, NULL
    )
  )
})
