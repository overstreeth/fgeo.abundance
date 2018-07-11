context("neighbor")

set.seed(1)
tree <- fgeo.data::luquillo_tree5_random[1:10, ]

describe("neighbor_count() and neighbor_basal_area()", {
  it("outputs the equivalent to ctfs::NeighborDensityes() counterpart", {
    skip_if_not_installed("ctfs")
    ctfs_n <- ctfs::NeighborDensities(
      tree, r = 20, mindbh = 0, type = "count", include = unique(tree$status)
    )
    ctfs_b <- ctfs::NeighborDensities(
      tree, r = 20, mindbh = 0, type = "basal", include = unique(tree$status)
    )
    expect_known_output(ctfs_n, "ref-NeighbourDensities_count", print = TRUE)
    expect_known_output(ctfs_b, "ref-NeighbourDensities_basal", print = TRUE)
    
    fgeo_n <- neighbor_count(tree, r = 20, plotdim = c(1000, 500))
    fgeo_b <- neighbor_basal_area(tree, r = 20, plotdim = c(1000, 500))
    expect_equal(fgeo_n, ctfs_n)
    expect_equal(fgeo_b, ctfs_b)
  })
})
