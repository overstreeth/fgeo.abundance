# flags multiple stems
# flags multiple censusid
# flags multiple plot

# counts by groups
# returns a grouped dataset
# returns one group less than the main input
# input and output groups have the same case


context("abundance")

library(dplyr)

describe("abundance", {
  skip_if_not_installed("ctfs")
  abun_ctfs <- function(x, ...) ctfs::abundance(x, ...)$abund$all
  tree_id <- function(x) {
    tibble(
      treeID = as.character(x), 
      dbh = 1, 
      status = "A", 
      date = 800
    )
  }
  
  it("behaves as ctfs::abundance()", {
    expect_ctfs_equal_to_fgeo <- function(ids) {
      expect_equal(abun_ctfs(tree_id(ids)), abundance(tree_id(ids))$n)
    }
    expect_ctfs_equal_to_fgeo(ids = 1)
    expect_ctfs_equal_to_fgeo(ids = 1:2)
    
    # Grouped summaries return similar (but are implemented diferently)
    tree <- mutate(tree_id(1:2), sp = letters[1:2])
    expect_equal(
      abun_ctfs(tree, split1 = tree$sp), 
      abundance(group_by(tree, sp))$n
    )
  })
  
  it("Returns consistent output with cero row dataframe", {
    cero_row_dfm <- tree_id(1)[0, ]
    expect_equal(abundance(cero_row_dfm)$n, 0)
    # ctfs's version resuls in dataframe with 0 rows and 0 columns
    expect_equal(abun_ctfs(cero_row_dfm), NULL)
  })
  
  it("works with both census and ViewFullTable", {
    expect_equal(
      abundance(tree_id(1)),
      abundance(rename(tree_id(1), TreeID = treeID, DBH = dbh))
    )
  })
  
  it("warns multiple stems", {
    expect_warning(
      abundance(mutate(tree_id(1:2), stemID = c("1.1", "1.2"))),
      "Multiple.*stemid"
    )
  })
})
