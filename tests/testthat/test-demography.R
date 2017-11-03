context("test-demography.R")

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

test_that("demography_df works as expected", {
  out_r <- recruitment_df(tiny1, tiny2)
  expect_is(out_r, "data.frame")

  out_m <- mortality_df(tiny1, tiny2)
  expect_is(out_m, "data.frame")
  
  expect_warning(out_m2 <- mortality_df(tiny1, tiny2, split1 = tiny1$sp))
  expect_is(out_m2, "data.frame")
  
  # errs bc *_df() does supports no more than one split variable
  expect_error(
    recruitment_df(tiny1, tiny2, split1 = tiny1$sp, split2 = tiny1$quadrat)
  )
})
