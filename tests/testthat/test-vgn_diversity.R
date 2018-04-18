context("vgn_diversity.R")

library(dplyr)

cns <- data.frame(
  CensusID = factor(rep(c(1, 2), 3)),
  quadrat = "0000",
  sp = rep(paste0("sp", 1:3), 2),
  n = sample.int(100, 6)
)

test_that("outputs a dataframe", {
  expect_silent(
    result <- vgn_diversity(cns, n)
  )
  expect_type(result, "list")
  expect_true(has_class_df(result))
})

test_that("groups as expected", {
  ungrouped <- vgn_diversity(cns, n)
  expect_length(ungrouped, 2)
  expect_equal(names(ungrouped), c("index", "value"))
  
  grouped_by_one <- cns %>% 
    group_by(CensusID) %>% 
    vgn_diversity(n)
  expect_length(grouped_by_one, 3)

  grouped_by_two <- cns %>% 
    group_by(CensusID, sp) %>% 
    vgn_diversity(n)
  expect_length(grouped_by_two, 4)
})

test_that("fails with informative error", {
  expect_error(
    vgn_diversity(cns),
    "is missing"
  )
  expect_error(vgn_diversity(col = n))
})

test_that("output is equal to vegan", {
  div <- vgn_diversity(cns, n)
  shan <- vegan::diversity(cns$n, "shannon")
  simp <- vegan::diversity(cns$n, "simpson")
  invs <- vegan::diversity(cns$n, "invsimpson")
  pull_metric <- function(x, metric) {
    dplyr::filter(x, index == metric)$value
  }
  
  expect_true(dplyr::near(pull_metric(div, "shannon"), shan))
  expect_true(dplyr::near(pull_metric(div, "simpson"), simp))
  expect_true(dplyr::near(pull_metric(div, "invsimpson"), invs))
})
