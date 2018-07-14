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
  
describe("collapse_treeid()", {
  it("outputs the same groups as input", {
    # Ungrouped
    collapsed <- collapse_treeid(cns)
    expect_equal(group_vars(collapsed), group_vars(cns))
    
    # Grouped
    bysp <- group_by(cns, sp)
    collapsed <- collapse_treeid(bysp)
    expect_equal(group_vars(collapsed), group_vars(bysp))
  })
  
  it("automatically groups by CensusID", {
    cns$CensusID <- c(1, 2, 1, 2, 2)
    .cns <- arrange(cns, CensusID, treeID, stemID, dbh)
    out <- collapse_treeid(.cns)
    out <- arrange(out, CensusID, treeID, stemID, dbh)
    # Output all available censuses
    expect_equal(out$CensusID, as.double(c(1, 1, 2, 2)))
    # Output only one tree per census
    expect_equal(out$treeID, as.character(c(1, 2, 1, 2)))
    # Ouput largest stem per tree per census
    expect_equal(out$stemID, as.character(c(1.1, 2.1, 1.2, 2.2)))
    
    # It picks an oviously larger stem added to census 2
    cns$CensusID <- c(1, 2, 1, 2, 2)
    .cns2 <- bind_rows(
      .cns, 
      list(dbh = 200, sp = "sp2", treeID = "2", stemID = "2.4", CensusID = 2)
    )
    out2 <- collapse_treeid(.cns2)
    picked <- filter(out2, CensusID == 2, treeID == 2)
    expect_equal(picked$stemID, "2.4")
    expect_equal(picked$dbh, 200)
  })
  
  it("drops missing values of censusid if there are multiple unique censusid", {
    # Doesn't drop missing censusid if they are unambiguous (only one censusid)
    cns$CensusID <- c(1, 1, 1, 1, NA)
    expect_silent(out <- collapse_treeid(cns))
    
    # Drops missing censusid if they are unambiguous (multiple censusid)
    cns$CensusID <- c(1, 1, 2, 2, NA)
    expect_warning(
      out <- collapse_treeid(cns),
      "Dropping.*rows with missing.*values"
    )
    
    expect_false(any(is.na(out$CensusID)))
  })
  
  it("rejects data with multiple values of `plotname`", {
    cns$PlotName <- c(1, 1, 2, 2, NA)
    expect_error(
      out <- collapse_treeid(cns),
      "must have a single plotname"
    )
  })
})

