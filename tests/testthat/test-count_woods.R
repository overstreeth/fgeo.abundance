context("count_woods")

library(dplyr)

cns <- tibble::tribble(
  ~dbh,   ~sp, ~treeID, ~stemID,
    10, "sp1",     "1",   "1.1",
   100, "sp1",     "1",   "1.2",
    22, "sp2",     "2",   "2.1",
    99, "sp2",     "2",   "2.2",
    NA, "sp2",     "2",   "2.3"
)

describe("count_woods() works with ViewFullTable", {
  it("doesn't err with vft names", {
    vft <- rename(cns, DBH = dbh, TreeID = treeID, StemID = stemID)
    out1 <- expect_silent(count_woods(vft, dbh > 0))
    out2 <- expect_silent(count_woods(cns, dbh > 0))
  })
})

describe("count_woods() outputs", {
  it("outputs the expected data structure", {
    # Ungrouped, count all woods
    out <- count_woods(cns, dbh >= 0)
    expect_is(out, "data.frame")
    expect_named(out, "n")
    expect_length(out, 1)
    expect_length(nrow(out), 1)
    expect_equal(out$n, 2)
    
    # Ungrouped, count trees
    out <- count_woods(cns, dbh >= 100)
    expect_equal(out$n, 1)
    expect_equal(out, count_trees(cns))

    # Ungrouped, count saplings
    out <- count_woods(cns, dbh >= 10, dbh < 100)
    expect_equal(out$n, 1)
    expect_equal(out, count_saplings(cns))
  })
  
  it("works with grouped data", {
    by_sp <- group_by(cns, sp)
    out <- count_woods(by_sp, dbh >= 0)
    expect_is(out, "data.frame")
    expect_named(out, c("sp", "n"))
    expect_length(out, 2)
    expect_equal(nrow(out), 2)
    expect_equal(out$n, c(1, 1))
    
    by_sp <- group_by(cns, sp)
    out <- count_trees(by_sp)
    expect_equal(out, tibble::tibble(sp = c("sp1", "sp2"), n = c(1L, 0L)))

    out <- count_saplings(by_sp)
    expect_equal(out, tibble::tibble(sp = c("sp1", "sp2"), n = c(0L, 1L)))
  })
})

describe("count_woods() inputs", {
  it("fails with informative message", {
    expect_error(count_woods("bad"), "data.frame.*is not TRUE")
  })
})

describe("count_woods() features", {
  cns <- tibble::tribble(
    ~dbh,   ~sp, ~treeID, ~stemID, ~CensusID,
      10, "sp1",     "1",   "1.1",        1,
     100, "sp1",     "1",   "1.2",        1,
      22, "sp2",     "2",   "2.1",        1,
      99, "sp2",     "2",   "2.2",        1,
      NA, "sp2",     "2",   "2.3",        1,

      10, "sp1",     "1",   "1.1",        2,
     100, "sp1",     "1",   "1.2",        2,
      22, "sp2",     "2",   "2.1",        2,
      99, "sp2",     "2",   "2.2",        2,
      NA, "sp2",     "2",   "2.3",        2
  )
  
  it("allows filtering via ... using variables other than `dbh`", {
    out <- count_woods(cns, sp == "sp1")
    expect_equal(out$n, c(1, 1))
  })
  
  it("works automatically groups by censusid if census id exists", {
    # All woods inlcuded
    out <- count_woods(cns)
    expect_named(out, c("CensusID", "n"))
    expect_equal(out$n, c(2, 2))
    
    out <- count_trees(cns)
    expect_equal(out$n, c(1, 1))
    
    out <- count_saplings(cns)
    expect_equal(out$n, c(1, 1))
  })
  
  it("outputs the original groups", {
    by_sp <- group_by(cns, sp)
    out <- count_woods(by_sp)
    expect_equal(group_vars(out), group_vars(by_sp))
    
    by_sp_treeid <- group_by(cns, sp, treeID)
    out <- count_woods(by_sp_treeid)
    expect_equal(group_vars(out), group_vars(by_sp_treeid))
  })
  
})
