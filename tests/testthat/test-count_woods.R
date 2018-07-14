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
    bysp <- group_by(cns, sp)
    out <- count_woods(bysp, dbh >= 0)
    expect_is(out, "data.frame")
    expect_named(out, c("sp", "n"))
    expect_length(out, 2)
    expect_equal(nrow(out), 2)
    expect_equal(out$n, c(1, 1))
    
    bysp <- group_by(cns, sp)
    out <- count_trees(bysp)
    expect_equal(out, tibble::tibble(sp = "sp1", n = 1L))

    out <- count_saplings(bysp)
    expect_equal(out, tibble::tibble(sp = "sp2", n = 1L))
  })
})

describe("count_woods() inputs", {
  it("fails with informative message", {
    expect_error(count_woods("bad"), "data.frame.*is not TRUE")
  })
})
