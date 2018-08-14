context("abundance_byyr")

# All moved to byyr2().

context("basal_area_byyr")

library(dplyr)
library(rlang)
library(fgeo.tool)
library(fgeo.base)

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
    fgeo.tool::pick_largest_hom_dbh() %>% 
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
