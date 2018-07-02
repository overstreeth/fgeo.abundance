context("byyr_abundance")

test_that("outputs as expected", {
  skip_if_not_installed("fgeo.tool")
  skip_if_not_installed("readr")
  path <- "byyr_toy_vft.csv"
  # path <- "tests/testthat/byyr_toy_vft.csv"
  vft <- readr::read_csv(path)

  # All trees are of the same species. There are two trees, each with two stems.
  # In census 1, the count of alive trees should be 2 because both trees are alive,
  #   but note that one stem is dead (StemID = 1.2).
  # In census 2 the count of alive trees should be 1:
  #   * One tree is alive (TreeID = 1) although one stem is gone (StemID = 1.2);
  #   * One tree is dead (TreeID = 2) because both its stems are dead.
  vft <- vft %>% 
    fgeo.tool::add_status_tree(status_a = "alive", status_d = "dead") %>%
    fgeo.tool::drop_dead_tree(.status = "dead")
  
  out <- byyr_abundance(vft)
  expect_named(set_names(out, tolower), c("species", "family", "2001", "2002"))
  expect_equal(out$`2001`, 2)
  expect_equal(out$`2002`, 1)
})



test_that("works with luquillo", {
  skip_if_not_installed("luquillo")
  suppressMessages(
    suppressWarnings({
      luq <- luquillo::ViewFullTable_luquillo %>% 
        dplyr::sample_n(10) %>% 
        fgeo.base::pick_plotname("luquillo") %>% 
        fgeo.base::pick_dbh_min(2)
      out <- byyr_abundance(luq)
    })
  )
  expect_is(out, "data.frame")
})

vft <- tibble::tibble(
  TreeID = rep(c("0001", "0002", "0003", "0004"), 2),
  Tag = rep(c("0001", "0002", "0003", "0004"), 2),
  PlotName = rep("pnm", 8),
  Status = rep(c("dead", "alive", "broken below", "missing"), 2),
  DBH = 1:8,
  ExactDate = paste0(rep(c(2000, 2001), each = 4), "-01-01"),
  PlotCensusNumber = rep(1:2, each = 4),
  CensusID = PlotCensusNumber,
  Genus = LETTERS[1:8],
  SpeciesName = letters[1:8],
  Family = "fmly"
)

test_that("has expected structure", {
  out <- suppressWarnings(suppressMessages(byyr_abundance(vft)))
  expect_is(out, "data.frame")
  expect_named(out, c("species", "Family", "2000", "2001"))
})

test_that("fails with informative error", {
  expect_error(byyr_abundance(1))
  expect_error(byyr_abundance())
})

tiny <- tibble::tibble(
  Tag = c("0001", "0001", "0002", "0002", "0003", "0003"),
  TreeID = c("0001", "0001", "0002", "0002", "0003", "0003"),
  PlotName = c("p", "p", "p", "p", "p", "p"),
  Status = c("alive", "alive", "alive", "alive", "alive", "alive"),
  DBH = c(1L, 5L, 2L, 6L, 2L, 6L),
  ExactDate =  c(
    "2000-01-01", "2001-01-01", "2000-01-01", "2001-01-01", "2000-01-01", 
    "2001-01-01"
  ),
  PlotCensusNumber = c(1L, 2L, 1L, 2L, 1L, 2L),
  CensusID = c(1L, 2L, 1L, 2L, 1L, 2L),
  Genus = c("A", "A", "B", "B", "B", "B"),
  SpeciesName = c("a", "a", "b", "b", "b", "b"),
  Family = c("f", "f", "f", "f", "f", "f")
)

test_that("known output", {
  out1 <- suppressWarnings(suppressMessages(byyr_abundance(tiny)))
  expect_equal(out1$`2000`, c(1, 2))
  expect_equal(out1$`2001`, c(1, 2))
  
  tiny2 <- tiny[1:4, ]
  out2 <- suppressWarnings(suppressMessages(byyr_abundance(tiny2)))
  expect_equal(out2$`2000`, c(1, 1))
  expect_equal(out2$`2001`, c(1, 1))

  tiny3 <- tiny[c(1, 3, 4), ]
  out3 <- suppressWarnings(suppressMessages(byyr_abundance(tiny3)))
  expect_equal(out3$`2000`, c(1, 1))
  expect_equal(out3$`2001`, c(0, 1))
})



context("byyr_basal_area")

test_that("works with data from site Luquillo", {
  suppressMessages(
    suppressWarnings({
      luq <- fgeo.data::luquillo_vft_4quad %>% 
        dplyr::sample_n(10) %>% 
        fgeo.base::pick_plotname("luquillo") %>% 
        fgeo.base::pick_dbh_min(2)
      out <- byyr_basal_area(luq)
    })
  )
  expect_is(out, "data.frame")
})

vft <- tibble::tibble(
  Tag = rep(c("0001", "0002", "0003", "0004"), 2),
  PlotName = rep("pnm", 8),
  Status = rep(c("dead", "alive", "broken below", "missing"), 2),
  DBH = 1:8,
  ExactDate = paste0(rep(c(2000, 2001), each = 4), "-01-01"),
  PlotCensusNumber = rep(1:2, each = 4),
  CensusID = PlotCensusNumber,
  Genus = LETTERS[1:8],
  SpeciesName = letters[1:8],
  Family = "fmly"
)

test_that("has expected structure", {
  out <- suppressWarnings(suppressMessages(byyr_basal_area(vft)))
  expect_is(out, "data.frame")
  expect_named(out, c("species", "Family", "2000", "2001"))
})

test_that("fails with informative error", {
  expect_error(byyr_basal_area(1))
  expect_error(byyr_basal_area())
})

tiny <- tibble::tibble(
  Tag = c("0001", "0001", "0002", "0002"),
  PlotName = c("p", "p", "p", "p"),
  Status = c("alive", "alive", "alive", "alive"),
  DBH =   c(1L, 1L, 1L, 1L),
  ExactDate =   c("2000-01-01", "2001-01-01", "2000-01-01", "2001-01-01"),
  PlotCensusNumber = c(1L, 2L, 1L, 2L),
  CensusID = c(1L, 2L, 1L, 2L),
  Genus = c("A", "A", "B", "B"),
  SpeciesName = c("a", "a", "b", "b"),
  Family = c("f", "f", "f", "f")
)

test_that("known output", {
  out1 <- suppressWarnings(suppressMessages(byyr_basal_area(tiny)))
  expect_equal(out1$`2000`, c(basal_area(1), basal_area(1)))
  expect_equal(out1$`2001`, c(basal_area(1), basal_area(1)))
  
  tiny2 <- tiny
  tiny2$DBH <- c(1, 10, 2, 20)
  out2 <- suppressWarnings(suppressMessages(byyr_basal_area(tiny2)))
  expect_equal(out2$`2000`, c(basal_area(1), basal_area(2)))
  expect_equal(out2$`2001`, c(basal_area(10), basal_area(20)))
})

test_that("counts multi-stem trees correctly", {
  library(fgeo.tool)
  library(fgeo.base)
  
  vft <- data.frame(
    PlotName = "p",
    PlotCensusNumber = c(1, 1, 2, 2, 1, 2),
    CensusID = c(1, 1, 2, 2, 1, 2),
    ExactDate = c(
      "2001-01-01", "2001-01-01", "2002-01-01", "2002-01-01",
      "2001-01-01", "2002-01-01"
    ),
    Tag = c("0001", "0001", "0001", "0001", "0002", "0002"),
    TreeID = c("0001", "0001", "0001", "0001", "0002", "0002"),
    SteemID = c(  "1",    "2",    "1",    "2",    "1",    "1"),
    Status = c("alive", "alive", "alive", "dead", "alive", "dead"),
    DBH = c(11, 15, 12, NA, 21, NA),
    Genus = c("A", "A", "A", "A", "B", "B"),
    SpeciesName = c("a", "a", "a", "a", "b", "b"),
    Family = "f",
    stringsAsFactors = FALSE
  )
  vft
  
  # First pick the data you want
  pick1 <- fgeo.base::pick_plotname(vft, "p")
  pick2 <- drop_dead_trees_by_cns(pick1)
  pick3 <- pick_dbh_min(pick2, 10)
  pick3 %>% arrange(PlotCensusNumber, TreeID)
  
  # Expected:
  # year 2001: A a = 1
  # year 2001: B b = 1
  # year 2002: A a = 1
  # year 2002: B b = 0
  # FIXME: Don't count stems but distinct tres with summarize(n = n_distinct(TreeID))
  out <- byyr_abundance(pick3)
  expect_equal(out$`2001`, c(1, 1))
  expect_equal(out$`2002`, c(1, 0))
})
