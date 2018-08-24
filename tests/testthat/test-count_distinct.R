context("count_distinct")

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

describe("count_distinct()", {
  it("counts as expected and outputs the expected names and groups", {
    expect_equal(pull(count_distinct(census, stemID), n), 6)
  
    by_treeid <- group_by(census, treeID)
    out <- count_distinct(by_treeid, stemID)
    expect_named(out, c("treeID", "n"))
    expect_equal(pull(out, n), c(2, 1, 3))
    expect_equal(group_vars(out), character(0))
    
    by_quad <- group_by(census, quadrat)
    expect_equal(pull(count_distinct(by_quad, stemID), n), c(3, 3))
    expect_equal(pull(count_distinct(by_quad, treeID), n), c(2, 1))

    by_treeid_sp <- group_by(census, treeID, sp)
    out <- count_distinct(by_treeid_sp, stemID)
    expect_equal(group_vars(out), "treeID")
  })
})

describe("count_distinct_stemid()", {
  it("counts as expected", {
    by_sp <- group_by(census, sp)
    out <- count_distinct_stemid(by_sp)
    expect_equal(pull(out, n), c(2, 1, 3))
    
    by_quad <- group_by(census, quadrat)
    expect_equal(pull(count_distinct_stemid(by_quad), n), c(3, 3))
    
    by_quad_sp <- group_by(census, quadrat, sp)
    expect_equal(pull(count_distinct_stemid(by_quad_sp), n), c(2, 1, 3))
  })
  
  it("outputs expected names and groups", {
    by_treeid_sp <- group_by(census, treeID, sp)
    out <- count_distinct_stemid(by_treeid_sp)
    expect_named(out, c("treeID", "sp", "n"))
    expect_equal(group_vars(out), "treeID")
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
    expect_error(count_distinct_treeid(census), "Duplicated values")
  })
  
  it("counts as expected with unique values per group", {
    # treeid is unique within each stemid-group
    expect_silent(out <- count_distinct_treeid(group_by(census, stemID)))
    expect_equal(nrow(out), length(unique(census$stemID)))
    expect_equal(unique(out$n), 1)
  })
  
  it("warns if detects multiple censusid", {
    census <- tibble(CensusID = c(1, 2), treeID = c(1, 2))
    expect_warning(count_distinct_treeid(census), "multiple values of censusid")
  })
  
  it("silent if censusid is unique within each group", {
    census <- tibble(CensusID = c(1, 2), treeID = c(1, 2))
    by_censusid <- group_by(census, CensusID)
    expect_silent(count_distinct_treeid(by_censusid))
  })
  
  it("outputs expected names and groups", {
    by_stemid_sp <- group_by(census, stemID, sp)
    out <- count_distinct_treeid(by_stemid_sp)
    expect_named(out, c("stemID", "sp", "n"))
    expect_equal(group_vars(out), "stemID")
  })
})
