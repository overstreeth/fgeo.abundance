context("count_distinct")

library(dplyr)

census <- tibble::tibble(
  treeID = c(1, 1, 2, 3, 3, 3),
  stemID = c(1, 2, 3, 4, 5, 6),
  quadrat = rep(1:2, each = 3),
  sp = c(paste0("sp", c(1, 1, 2)), paste0("sp", c(3, 3, 3))),
  dbh = 1
)
census

test_that("counts as expected", {
  expect_equal(pull(count_distinct(census, stemID), n), 6)

  by_treeid <- group_by(census, treeID)
  out <- count_distinct(by_treeid, stemID)
  expect_named(out, c("treeID", "n"))
  expect_equal(pull(out, n), c(2, 1, 3))

  by_quad <- group_by(census, quadrat)
  expect_equal(pull(count_distinct(by_quad, stemID), n), c(3, 3))
  expect_equal(pull(count_distinct_stemid(by_quad), n), c(3, 3))
  expect_equal(pull(count_distinct(by_quad, treeID), n), c(2, 1))
  expect_equal(pull(count_distinct_treeid(by_quad), n), c(2, 1))

  by_sp <- group_by(census, sp)
  expect_equal(pull(count_distinct_stemid(by_sp), n), c(2, 1, 3))
  expect_equal(pull(count_distinct_treeid(by_sp), n), c(1, 1, 1))

  by_quad_sp <- group_by(census, quadrat, sp)
  expect_equal(pull(count_distinct_stemid(by_quad_sp), n), c(2, 1, 3))
  expect_equal(pull(count_distinct_treeid(by_quad_sp), n), c(1, 1, 1))
})

test_that("fails with informative error", {
  expect_error(count_distinct(1), "must be a dataframe")
  expect_error(count_distinct_treeid(1), "must be a dataframe")
})
