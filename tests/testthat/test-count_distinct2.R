context("count_distinct_treeid2")

library(dplyr)
library(tibble)
library(datasets)

census <- tibble(
  treeID = c(1, 1, 2, 3, 3, 3),
  stemID = c(1, 2, 3, 4, 5, 6),
  quadrat = rep(1:2, each = 3),
  sp = c(paste0("sp", c(1, 1, 2)), paste0("sp", c(3, 3, 3))),
  dbh = 1
)
census

test_that("Counts minimal data", {
  dfm <- tibble(treeID = 1)
  expect_equal(count_distinct_treeid2(dfm)$n, 1)
  expect_equal(count_distinct_treeid2(dfm)$n, count_distinct_treeid(dfm)$n)
  
  dfm <- tibble(treeID = 1:2)
  expect_equal(count_distinct_treeid2(dfm)$n, 2)
  
  dfm <- tibble(treeID = c(NA))
  expect_equal(count_distinct_treeid2(dfm)$n, 1)
})

test_that("Warns if treeid is duplicated, and plotname/census id is multiple", {
  dfm <- tibble(treeID = c(1, 1))
  expect_warning(count_distinct_treeid2(dfm), "`treeid`: Duplicated")
  
  dfm <- tibble(treeID = c(1, 2), plotname = 1:2)
  expect_warning(count_distinct_treeid2(dfm), "`plotname`: Multiple")
  
  dfm <- tibble(treeID = c(1, 2), censusID = 1:2)
  expect_warning(count_distinct_treeid2(dfm), "`censusid`: Multiple")
  
  expect_equal(count_distinct_treeid2(group_by(dfm, censusID))$n, c(1, 1))
})
