context("count_distinct")

library(dplyr)
library(tibble)

census <- tibble(
  treeID = c(1, 1, 2, 3, 3, 3),
  stemID = c(1, 2, 3, 4, 5, 6),
  quadrat = rep(1:2, each = 3),
  sp = c(paste0("sp", c(1, 1, 2)), paste0("sp", c(3, 3, 3))),
  dbh = 1
)
census

describe("count_distinct()", {
  by_treeid <- group_by(census, treeID)
  by_quad <- group_by(census, quadrat)

  it("count_distinct() counts as expected", {
    expect_equal(pull(count_distinct(census, stemID), n), 6)
  
    out <- count_distinct(by_treeid, stemID)
    expect_named(out, c("treeID", "n"))
    expect_equal(pull(out, n), c(2, 1, 3))
    expect_equal(pull(count_distinct(by_quad, stemID), n), c(3, 3))
    expect_equal(pull(count_distinct(by_quad, treeID), n), c(2, 1))
  })
  
  it("count_distinct_stemid() counts as expected", {
    by_sp <- group_by(census, sp)
    expect_equal(pull(count_distinct_stemid(by_quad), n), c(3, 3))
    expect_equal(pull(count_distinct_stemid(by_sp), n), c(2, 1, 3))
  
    by_quad_sp <- group_by(census, quadrat, sp)
    expect_equal(pull(count_distinct_stemid(by_quad_sp), n), c(2, 1, 3))
  })
})

test_that("count_distinct() and friends fail with informative error", {
  expect_error(count_distinct(1), "must be a dataframe")
  expect_error(count_distinct(mtcars), ".var` must be supplied")
  expect_error(count_distinct(mtcars, "bad .var"), "must be a variable of")
  expect_error(count_distinct_treeid(1), "must be a dataframe")
})

describe("count_distinct_treeid()", {
  
  it("errs if detects multiple treeid", {
    census <- tibble(treeID = c(1, 1), stemID = c(1, 2))
    expect_error(
      count_distinct_treeid(census),
      "Detected duplicated values"
    )
  })
  
  it("counts as expected with unique values per group", {
    # treeid is unique within each stemid-group
    expect_silent(out <- count_distinct_treeid(group_by(census, stemID)))
    expect_equal(nrow(out), length(unique(census$stemID)))
    expect_equal(unique(out$n), 1)
  })
  
  it("warns if detects multiple censusid", {
    census <- tibble(CensusID = c(1, 2), treeID = c(1, 2))
    expect_warning(count_distinct_treeid(census))
  })
})
