context("abundance_byyr")

library(dplyr)
library(rlang)
library(fgeo.tool)
library(fgeo.base)

test_that("outputs as expected", {
  skip_if_not_installed("fgeo.tool")
  skip_if_not_installed("readr")
  vft <- readr::read_csv(test_path("data-byyr_toy_vft.csv"))

  # All trees are of the same species. There are two trees, each with two stems.
  # In census 1, the count of alive trees should be 2 because both trees are
  # alive, but note that one stem is dead (StemID = 1.2). In census 2 the count
  # of alive trees should be 1:
  #   * One tree is alive (TreeID = 1) although one stem is gone (StemID = 1.2);
  #   * One tree is dead (TreeID = 2) because both its stems are dead.
  
  # Collapse treeid
  vft <- pick_largest_hom_dbh(vft)
  out <- abundance_byyr(vft)
  expect_named(set_names(out, tolower), c("species", "family", "2001", "2002"))
  expect_equal(out$`2001`, 2)
  expect_equal(out$`2002`, 1)
})

test_that("works with luquillo", {
  skip_if_not_installed("luquillo")
  suppressMessages(
    suppressWarnings({
      luq <- luquillo::ViewFullTable_luquillo %>%
        sample_n(10) %>%
        pick_plotname("luquillo") %>%
        pick_largest_hom_dbh() %>%
        pick_dbh_min(2)
      out <- abundance_byyr(luq)
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
  out <- suppressWarnings(suppressMessages(abundance_byyr(vft)))
  expect_is(out, "data.frame")
  expect_named(out, c("species", "Family", "2000", "2001"))
})

test_that("fails with informative error", {
  expect_error(abundance_byyr(1))
  expect_error(abundance_byyr())
})

tiny <- tibble::tibble(
  Tag = c("0001", "0001", "0002", "0002", "0003", "0003"),
  TreeID = c("0001", "0001", "0002", "0002", "0003", "0003"),
  PlotName = c("p", "p", "p", "p", "p", "p"),
  Status = c("alive", "alive", "alive", "alive", "alive", "alive"),
  DBH = c(1L, 5L, 2L, 6L, 2L, 6L),
  ExactDate = c(
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
  out1 <- suppressWarnings(suppressMessages(abundance_byyr(tiny)))
  expect_equal(out1$`2000`, c(1, 2))
  expect_equal(out1$`2001`, c(1, 2))

  tiny2 <- tiny[1:4, ]
  out2 <- suppressWarnings(suppressMessages(abundance_byyr(tiny2)))
  expect_equal(out2$`2000`, c(1, 1))
  expect_equal(out2$`2001`, c(1, 1))

  tiny3 <- tiny[c(1, 3, 4), ]
  out3 <- suppressWarnings(suppressMessages(abundance_byyr(tiny3)))
  expect_equal(out3$`2000`, c(1, 1))
  expect_equal(out3$`2001`, c(0, 1))
})



context("basal_area_byyr")

test_that("works with data from site Luquillo", {
  suppressMessages(
    suppressWarnings({
      luq <- fgeo.data::luquillo_vft_4quad %>%
        sample_n(10) %>%
        pick_plotname("luquillo") %>%
        pick_dbh_min(2)
      out <- basal_area_byyr(luq)
    })
  )
  expect_is(out, "data.frame")
})

vft <- tibble::tibble(
  Tag = rep(c("0001", "0002", "0003", "0004"), 2),
  TreeID = rep(c("0001", "0002", "0003", "0004"), 2),
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
  out <- suppressWarnings(suppressMessages(basal_area_byyr(vft)))
  expect_is(out, "data.frame")
  expect_named(out, c("species", "Family", "2000", "2001"))
})

test_that("fails with informative error", {
  expect_error(basal_area_byyr(1))
  expect_error(basal_area_byyr())
})

tiny <- tibble::tibble(
  Tag = c("0001", "0001", "0002", "0002"),
  TreeID = c("0001", "0001", "0002", "0002"),
  PlotName = c("p", "p", "p", "p"),
  Status = c("alive", "alive", "alive", "alive"),
  DBH = c(1L, 1L, 1L, 1L),
  ExactDate = c("2000-01-01", "2001-01-01", "2000-01-01", "2001-01-01"),
  PlotCensusNumber = c(1L, 2L, 1L, 2L),
  CensusID = c(1L, 2L, 1L, 2L),
  Genus = c("A", "A", "B", "B"),
  SpeciesName = c("a", "a", "b", "b"),
  Family = c("f", "f", "f", "f")
)

test_that("known output", {
  out1 <- suppressWarnings(suppressMessages(basal_area_byyr(tiny)))
  expect_equal(out1$`2000`, c(basal_area(1), basal_area(1)))
  expect_equal(out1$`2001`, c(basal_area(1), basal_area(1)))

  tiny2 <- tiny
  tiny2$DBH <- c(1, 10, 2, 20)
  out2 <- suppressWarnings(suppressMessages(basal_area_byyr(tiny2)))
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
    StemID = c("1", "2", "1", "2", "1", "1"),
    Status = c("alive", "alive", "alive", "dead", "alive", "dead"),
    DBH = c(11, 15, 12, NA, 21, NA),
    Genus = c("A", "A", "A", "A", "B", "B"),
    SpeciesName = c("a", "a", "a", "a", "b", "b"),
    Family = "f",
    HOM = rep(130, 6),
    stringsAsFactors = FALSE
  )
  vft <- tibble::as.tibble(vft)

  # Expected:
  # year 2001: A a = 1
  # year 2001: B b = 1
  # year 2002: A a = 1
  # year 2002: B b = 0
  out <- vft %>% 
    pick_dbh_min(10) %>% 
    pick_largest_hom_dbh() %>% 
    abundance_byyr()
  expect_equal(out$`2001`, c(1, 1))
  expect_equal(out$`2002`, c(1, 0))
})



context("inform_if_bad_status")

test_that("informs if bad status", {
  expect_silent(out <- inform_if_bad_status(data.frame(Status = "OK"), "OK"))
  expect_named(out, "Status")
  expect_message(
    inform_if_bad_status(data.frame(Status = "bad"), "OK"), "should match"
  )
})
