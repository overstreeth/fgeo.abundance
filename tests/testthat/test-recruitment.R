context("test-recruitment.R")

library(dplyr)

# Some data to play with
census1 <- forestr::bci12t6mini
census2 <- forestr::bci12t7mini
# 10x smaller data to save time
tiny1 <- dplyr::filter(census1, sp  %in% unique(census1$sp)[1:10])
tiny2 <- dplyr::filter(census2, sp  %in% unique(census1$sp)[1:10])
# Check the sp filtered for each data set are the same
expect_equal(
  tiny1$sp %>% unique, 
  tiny2$sp %>% unique
)

test_that("works with predictable inputs", {
  out1 <- recruitment(tiny1, tiny2)
  expect_type(out1, "list")
  expect_length(out1, 8)
  expect_false(any(is.na(out1)))
  
  # mindbh works: output is valid and different from before
  out2 <- recruitment(tiny1, tiny2, mindbh = 50)
  expect_type(out2, "list")
  expect_length(out2, 8)
  expect_false(any(is.na(out2)))
  expect_false(identical(out1, out2))
  
  # spliting by one works
  # expect warning because tiny2 has two species which dbh is all NA
  out3 <- expect_warning(recruitment(tiny1, tiny2, split1 = tiny1$sp))
  out4 <- expect_warning(recruitment(tiny1, tiny2, split1 = tiny2$sp))
  # output is valid
  expect_type(out3, "list")
  expect_length(out3, 8)
  expect_false(any(is.na(out3)))
  # spliting by sp of census 1 or census 2 is the same
  expect_true(identical(out3, out4))
  # and the result is different than not splitting at al 
  expect_false(identical(out1, out4))
  
  # Splitting by two works
  out5 <- expect_warning(
    recruitment(tiny1, tiny2, split1 = tiny1$sp, split2 = tiny1$quadrat)
  )
  # output is valid
  # now we still get a list but each element is a double
  expect_type(out5[[1]], "double")
})

test_that("with wrongly named crucial-variables throws warning", {
  crucial_nms <- c("dbh", "pom", "status", "date")

  newnm1 <- dplyr::rename(tiny1, xxx = dbh)
  newnm2 <- dplyr::rename(tiny2, xxx = dbh)
  expect_error(recruitment(newnm1, newnm2))

  newnm1 <- dplyr::rename(tiny1, xxx = pom)
  newnm2 <- dplyr::rename(tiny2, xxx = pom)
  expect_error(recruitment(newnm1, newnm2))

  newnm1 <- dplyr::rename(tiny1, xxx = status)
  newnm2 <- dplyr::rename(tiny2, xxx = status)
  expect_error(recruitment(newnm1, newnm2))

  newnm1 <- dplyr::rename(tiny1, xxx = date)
  newnm2 <- dplyr::rename(tiny2, xxx = date)
  expect_error(recruitment(newnm1, newnm2))
})
