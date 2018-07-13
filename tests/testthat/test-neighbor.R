context("neighbor")

library(dplyr)
library(purrr)
library(tidyr)

tree <- fgeo.data::luquillo_tree5_random[1:10, ]

describe("neighbor_*() inputs", {
  it("fails with informative messages when fed with common bad-inputs", {
    no_df <- 1
    expect_error(
      neighbor_count(no_df, r = 20, plotdim = c(320, 500)), 
      "is not TRUE"
    )
    expect_error(
      neighbor_count(tree, .subset = no_df, r = 20, plotdim = c(320, 500)),
      "is not TRUE"
    )
    bad_nm <- data.frame(bad_names = 1)
    expect_error(
      neighbor_count(bad_nm, r = 20, plotdim = c(320, 500)),
      "Invalid.*Ensure your data set has these variables"
    )
    expect_error(
      neighbor_count(tree, .subset = bad_nm, r = 20, plotdim = c(320, 500)),
      "Invalid.*Ensure your data set has these variables"
    )
    expect_error(
      neighbor_count(tree, plotdim = c(320, 500)),
      "argument.*is missing"
    )
  })
})

describe("neighbor_*() outputs", {
  skip_if_not_installed("ctfs")
  ctfs_n <- ctfs::NeighborDensities(
    tree, r = 20, mindbh = 0, type = "count", include = unique(tree$status)
  )
  ctfs_b <- ctfs::NeighborDensities(
    tree, r = 20, mindbh = 0, type = "basal", include = unique(tree$status)
  )
  it("returns the same output as in the original ctfs functoin", {
    expect_known_output(ctfs_n, "ref-NeighbourDensities_count", print = TRUE)
    expect_known_output(ctfs_b, "ref-NeighbourDensities_basal", print = TRUE)
  })

  fgeo_n <- neighbor_count(tree, r = 20, plotdim = c(320, 500))
  fgeo_b <- neighbor_basal_area(tree, r = 20, plotdim = c(320, 500))
  it("outputs equal to the original ctfs function", {
    expect_is(fgeo_n, "tbl")
    # Names and s3 class are intentionally different:
    # * Coercing to tibble.
    # * Removing names.
    expect_identical(unname(fgeo_n), unname(tibble::as.tibble(ctfs_n)))
    expect_is(fgeo_b, "tbl")
    expect_identical(unname(fgeo_b), unname(tibble::as.tibble(ctfs_b)))
  })
})

describe("neighbor_*() side effects", {
  it("outputs a suppressable message (not a `cat()` like notification)", {
    expect_silent(suppressMessages(neighbor_count(tree, r = 20)))
  })
})

describe("neighbor_*() features", {
  it("guesses `plotdim` if it's not provided", {
    expect_message(neighbor_count(tree, r = 20), "Gessing: plotdim")
  })
  
  it("works with grouped dataframe", {
    it("ouputs one row per row in the input", {
      tree <- fgeo.data::luquillo_tree5_random
      # Ungrouped
      t <- suppressMessages(neighbor_count(tree, r = 20))
      expect_equal(nrow(t), nrow(tree))
      
      smaller_quad <- fgeo.tool::pick_top(tree, quadrat, 5)
      
      byquad <- dplyr::group_by(smaller_quad, quadrat)
      q <- suppressMessages(neighbor_count(byquad, r = 20))
      expect_equal(nrow(q), nrow(byquad))
      expect_named(q, c("quadrat", names(t)))
      
      byspquad <- dplyr::group_by(smaller_quad, quadrat, sp)
      qs <- suppressMessages(neighbor_count(byspquad, r = 20))
      expect_equal(nrow(qs), nrow(byspquad))
      expect_named(qs, c("quadrat", "sp", names(t)))
      # Grouping by species makes isolates each species from all other.
      # * Expect zero hererospecific neighbors.
      expect_equal(unique(qs$heterospecific), 0)
    })
  })
})

describe("neighbor_*()", {
  quads <- pick_top(fgeo.data::luquillo_tree5_random, quadrat, 10)
  pd <- c(320, 500)
  r <- 20
  it("neighbor_count() outputs equal via group_by() and split()", {
    dfs <- split(quads, quads$quadrat)
    out_split <- reduce(lapply(dfs, neighbor_count, r = r, plotdim = pd), rbind)
    
    byquad <- group_by(quads, quadrat)
    out_neigh <- neighbor_count(byquad, r = r, plotdim = pd)
    out_neigh <- out_neigh[c("conspecific", "heterospecific")]
    
    expect_equal(out_neigh, out_split)
  })

  it("neighbor_basal_area() outputs equal via group_by() and split()", {
    dfs <- split(quads, quads$quadrat)
    out_split <- reduce(lapply(dfs, neighbor_count, r = r, plotdim = pd), rbind)
    
    byquad <- group_by(quads, quadrat)
    out_neigh <- neighbor_count(byquad, r = r, plotdim = pd)
    out_neigh <- out_neigh[c("conspecific", "heterospecific")]
    
    expect_equal(out_neigh, out_split)
  })
})

