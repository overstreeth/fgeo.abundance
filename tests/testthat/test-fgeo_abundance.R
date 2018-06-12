context("byyr_abundance")

test_that("works with luquillo", {
  suppressMessages(
    suppressWarnings({
      luq <- luquillo::ViewFullTable_luquillo %>% 
        dplyr::sample_n(10) %>% 
        pick_plotname("luquillo") %>% 
        fgeo.base::pick_dbh_min(2)
      out <- byyr_abundance(luq)
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
  out <- suppressWarnings(suppressMessages(byyr_abundance(vft)))
  expect_is(out, "data.frame")
  expect_named(out, c("species", "Family", "2000", "2001"))
})

test_that("fails with informative error", {
  expect_error(byyr_abundance(1))
  expect_error(byyr_abundance())

  suppressWarnings(suppressMessages({
    expect_error(
      byyr_abundance(vft, .valid_status = "bad input"), "failed to fix status"
    )
  }))
})



tiny <- tibble::tribble(
  ~Tag, ~PlotName, ~Status, ~DBH,   ~ExactDate, ~PlotCensusNumber, ~CensusID, ~Genus, ~SpeciesName, ~Family,
  "0001",     "p", "alive",   1L, "2000-01-01",                1L,        1L,    "A",          "a",     "f",
  "0001",     "p", "alive",   5L, "2001-01-01",                2L,        2L,    "A",          "a",     "f",
  "0002",     "p", "alive",   2L, "2000-01-01",                1L,        1L,    "B",          "b",     "f",
  "0002",     "p", "alive",   6L, "2001-01-01",                2L,        2L,    "B",          "b",     "f",
)

test_that("known output", {
  all_alive <- suppressWarnings(suppressMessages(byyr_abundance(tiny)))
  expect_equal(all_alive$`2000`, c(1, 1))
  expect_equal(all_alive$`2001`, c(1, 1))
  
  tiny_12d <- tiny
  tiny_12d[tiny_12d$Tag == "0001" & tiny_12d$CensusID == 2, ]$Status <- "dead"
  one_dead_in_cns2 <- suppressWarnings(suppressMessages(byyr_abundance(tiny_12d)))
  expect_equal(one_dead_in_cns2$`2000`, c(1, 1))
  expect_equal(one_dead_in_cns2$`2001`, c(0, 1))

  tiny_22d <- tiny
  tiny_22d[tiny_22d$Tag == "0002" & tiny_22d$CensusID == 2, ]$Status <- "dead"
  two_dead_in_cns2 <- suppressWarnings(suppressMessages(byyr_abundance(tiny_22d)))
  expect_equal(two_dead_in_cns2$`2000`, c(1, 1))
  expect_equal(two_dead_in_cns2$`2001`, c(1, 0))
})



context("byyr_basal_area")

test_that("works with luquillo", {
  suppressMessages(
    suppressWarnings({
      luq <- luquillo::ViewFullTable_luquillo %>% 
        dplyr::sample_n(10) %>% 
        pick_plotname("luquillo") %>% 
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
  
  suppressWarnings(suppressMessages({
    expect_error(
      byyr_basal_area(vft, .valid_status = "bad input"), "failed to fix status"
    )
  }))
})

tiny <- tibble::tribble(
  ~Tag, ~PlotName, ~Status, ~DBH,   ~ExactDate, ~PlotCensusNumber, ~CensusID, ~Genus, ~SpeciesName, ~Family,
  "0001",     "p", "alive",   1L, "2000-01-01",                1L,        1L,    "A",          "a",     "f",
  "0001",     "p", "alive",   1L, "2001-01-01",                2L,        2L,    "A",          "a",     "f",
  "0002",     "p", "alive",   1L, "2000-01-01",                1L,        1L,    "B",          "b",     "f",
  "0002",     "p", "alive",   1L, "2001-01-01",                2L,        2L,    "B",          "b",     "f",
)

test_that("known output", {
  alive <- suppressWarnings(suppressMessages(byyr_basal_area(tiny)))
  expect_equal(alive$`2000`, c(basal_area(1), basal_area(1)))
  expect_equal(alive$`2001`, c(basal_area(1), basal_area(1)))
  
  tiny_12d <- tiny
  tiny_12d[tiny_12d$Tag == "0001" & tiny_12d$CensusID == 2, ]$Status <- "dead"
  one_dead_in_cns2 <- suppressWarnings(suppressMessages(byyr_basal_area(tiny_12d)))
  
  expect_equal(one_dead_in_cns2$`2000`, c(basal_area(1), basal_area(1)))
  expect_equal(one_dead_in_cns2$`2001`, c(0, basal_area(1)))
  
  tiny_22d <- tiny
  tiny_22d[tiny_22d$Tag == "0002" & tiny_22d$CensusID == 2, ]$Status <- "dead"
  two_dead_in_cns2 <- suppressWarnings(suppressMessages(byyr_basal_area(tiny_22d)))
  expect_equal(two_dead_in_cns2$`2000`, c(basal_area(1), basal_area(1)))
  expect_equal(two_dead_in_cns2$`2001`, c(basal_area(1), 0))
})
