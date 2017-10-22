context("test-check.R")

census1 <- forestr::bci12t6mini
census2 <- forestr::bci12t7mini

test_that("check_if_all_dbh_is_na is silent or vocal as expected", {
  
  split1 = NULL
  expect_silent(check_if_all_dbh_is_na(cns = census1, split = split1))

  split1 = census1$quadrat
  expect_warning(check_if_all_dbh_is_na(cns = census1, split = split1))
})
