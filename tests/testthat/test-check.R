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
