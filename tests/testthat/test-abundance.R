library(dplyr)

context("basal_area2")

describe("basal_area2", {
  skip_if_not_installed("ctfs")
  ba_ctfs <- function(x, type = "ba", ...) ctfs::abundance(x, type, ...)$ba$all
  tree_id <- function(x) {
    tibble(
      treeID = as.character(x), 
      dbh = 1, 
      status = "A", 
      date = 800
    )
  }
  
  it("behaves as ctfs::abundance(type = 'ba')", {
    # FIXME: Why is this offset? Why it does not to appear in basal_area_byyr?
    offset <- 1000000
    # numeric
    expect_equal(basal_area2(tree_id(1))$basal_area, ba_ctfs(tree_id(1)) * offset)
    # data.frame
    expect_equal(
      basal_area2(tree_id(1:2))$basal_area, 
      ba_ctfs(tree_id(1:2)) * offset
    )

    # Grouped summaries return similar (but are implemented diferently)
    tree <- mutate(tree_id(1:2), sp = letters[1:2])
    expect_equal(
      basal_area2(group_by(tree, sp))$basal_area,
      ba_ctfs(tree, split1 = tree$sp) * offset
    )
    
    
    
  })
  
  
})
  



# abundance ---------------------------------------------------------------

context("abundance")

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
  
  it("warns duplicated treeid", {
    expect_warning(
      abundance(mutate(tree_id(c(1, 1)), stemID = c("1.1", "1.2"))),
      "treeid.*Duplicated values.*Do you need to pick main stems?"
    )
  })
    
  it("warns duplicated stemids", {
    # basal_area warns not treeid but stemid
    expect_warning(
      basal_area2(mutate(tree_id(c(1, 1)), stemID = c("1.1", "1.1"))),
      "stemid.*Duplicated values.*Do you need to pick largest.*hom.*values?"
    )
    expect_silent(basal_area2(tree_id(c(1, 1))))
  })
  
  it("warns multiple censusid", {
    expect_warning(
      abundance(mutate(tree_id(c(1, 1)), CensusID = c("1", "2"))),
      "censusid.*Multiple values.*Do you need to group by.*censusid?"
    )
    expect_warning(
       basal_area2(mutate(tree_id(c(1, 1)), CensusID = c("1", "2"))),
      "censusid.*Multiple values.*Do you need to group by.*censusid?"
    )
  })
  
  it("warns multiple plotname", {
    expect_warning(
      abundance(mutate(tree_id(c(1, 1)), PlotName = c("a", "b"))),
      "plotname.*Multiple values.*Do you need to pick a single plot?"
    )
    expect_warning(
      basal_area2(mutate(tree_id(c(1, 1)), PlotName = c("a", "b"))),
      "plotname.*Multiple values.*Do you need to pick a single plot?"
    )
  })
    
  it("doesn't change the case of input names or groups", {
    tree <- mutate(tree_id(1:2), CensusID = 1:2)
    abund <- abundance(group_by(tree, CensusID))
    expect_named(abund, c("CensusID", "n"))
    expect_equal(group_vars(abund), "CensusID")
  })
  
  it("returns groups of grouped data", {
    tree <- mutate(tree_id(1:2), CensusID = 1:2)
    expect_true(is_grouped_df(abundance(group_by(tree, CensusID))))
    expect_true(is_grouped_df(basal_area2(group_by(tree, CensusID))))
  })
  
})

