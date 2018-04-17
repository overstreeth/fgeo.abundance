context("smry_diversity.R")

library(dplyr)

cns <- data.frame(
  CensusID = factor(rep(c(1, 2), 3)),
  quadrat = "0000",
  sp = rep(paste0("sp", 1:3), 2),
  n = sample.int(100, 6)
)

test_that("outputs a dataframe", {
  expect_silent(
    result <- smry_diversity(cns, n)
  )
  expect_type(result, "list")
  expect_true(has_class_df(result))
})

test_that("groups as expected", {
  ungrouped <- smry_diversity(cns, n)
  expect_length(ungrouped, 2)
  expect_equal(names(ungrouped), c("diversity", "value"))
  
  grouped_by_one <- cns %>% 
    group_by(CensusID) %>% 
    smry_diversity(n)
  expect_length(grouped_by_one, 3)

  grouped_by_two <- cns %>% 
    group_by(CensusID, sp) %>% 
    smry_diversity(n)
  expect_length(grouped_by_two, 4)
})

test_that("fails with informative error", {
  expect_error(
    smry_diversity(cns),
    "`col` is missing"
  )
  expect_error(smry_diversity(col = n))
})
