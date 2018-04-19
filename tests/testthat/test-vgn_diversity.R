context("vgn_diversity.R")

library(dplyr)

cns <- data.frame(
  CensusID = factor(rep(c(1, 2), 3)),
  quadrat = "0000",
  sp = rep(paste0("sp", 1:3), 2),
  n = sample.int(100, 6)
)

test_that("outputs a dataframe", {
  expect_silent(
    result <- vgn_diversity(cns, n)
  )
  expect_type(result, "list")
  expect_true(has_class_df(result))
})

test_that("groups as expected", {
  ungrouped <- vgn_diversity(cns, n)
  expect_length(ungrouped, 2)
  expect_equal(names(ungrouped), c("index", "value"))
  
  grouped_by_one <- cns %>% 
    group_by(CensusID) %>% 
    vgn_diversity(n)
  expect_length(grouped_by_one, 3)

  grouped_by_two <- cns %>% 
    group_by(CensusID, sp) %>% 
    vgn_diversity(n)
  expect_length(grouped_by_two, 4)
})

test_that("fails with informative error", {
  not_dataframe <- 1
  expect_error(vgn_diversity(not_dataframe), "is not TRUE")
  expect_error(vgn_diversity(cns), "is missing")
  expect_error(vgn_diversity(abundance = n), "is missing")
})

test_that("output is equal to vegan", {
  div <- vgn_diversity(
    cns, n, c("shannon", "simpson", "invsimpson", "specnumber")
  )
  shan <- vegan::diversity(cns$n, "shannon")
  simp <- vegan::diversity(cns$n, "simpson")
  invs <- vegan::diversity(cns$n, "invsimpson")
  
  pull_metric <- function(x, metric) {
    dplyr::filter(x, index == metric)$value
  }
  expect_true(dplyr::near(pull_metric(div, "shannon"), shan))
  expect_true(dplyr::near(pull_metric(div, "simpson"), simp))
  expect_true(dplyr::near(pull_metric(div, "invsimpson"), invs))
})


test_that("specnumber is named", {
  census <- data.frame(
    quadrat = rep(c("0000", "0001"), each = 3),
    sp = rep(paste0("sp", 1:3), 2),
    n = sample.int(100, 6),
    stringsAsFactors = FALSE
  )
  out <- vgn_diversity(census, n)
  expect_true("specnumber" %in% out$index)
})

test_that("works with only one kind of diversity metric", {
  expect_silent(vgn_diversity(cns, n, index = "specnumber"))
  expect_silent(vgn_diversity(cns, n, index = "shannon"))
})

test_that("works with combined indices", {
  expect_silent(vgn_diversity(cns, n, index = c("specnumber", "shannon")))
  expect_silent(vgn_diversity(cns, n, index = c("shannon", "specnumber")))
})

test_that("outputs equal to vegan with different species accross groups", {
  census <- data.frame(
    quadrat = rep(c("0000", "0001"), each = 3),
    sp = paste0("sp", c(1:3, 1, 4, 5)),
    n = sample.int(100, 6),
    stringsAsFactors = FALSE
  )
  census <- census[1:5, ]

  ref <- census %>% 
    group_by(quadrat) %>% 
    mutate(
      specnumber = vegan::specnumber(n),
      shannon = vegan::diversity(n, "shannon"),
      simpson = vegan::diversity(n, "simpson"),
      invsimpson = vegan::diversity(n, "invsimpson")
    )
  
  indices <- c("shannon", "simpson", "invsimpson", "specnumber")
  out <- census %>% 
    group_by(quadrat) %>% 
    vgn_diversity(n, indices)
  
  compare_idx <- function(idx, out, ref) {
    setdiff(unique(ref[[idx]]), out[out$index == idx, ][["value"]])
  }
  difference <- length(unlist(lapply(indices, compare_idx, out, ref)))
  
  expect_equal(difference, 0)
})



# Compare with vegan ------------------------------------------------------

# Setup

library(tidyverse)
library(fgeo.abundance)
library(vegan)
data(BCI)

bci_wide <- BCI %>% 
  rowid_to_column() %>% 
  as.tibble()
bci_long <- gather(bci_wide, "sp", "n", -rowid)

# Funs

identical_diversity <- function(index, .f) {
  df_test_diversity(index, .f) %>% 
    pull() %>% 
    all()
}

df_test_diversity <- function(index, .f) {
  diversity_wide <- .f(bci_wide[-1], index)
  diversity_long <- vgn_diversity(group_by(bci_long, rowid), n, index)
  tibble(
    long = round(diversity_long$value, 1), 
    wide = round(diversity_wide, 1)
  ) %>% 
    mutate(is_equal = identical(long, wide))
}

# Tests

test_that("outputs equal to vegan::diversity()", {
  # vgn_diversity() outputs equal to vegan::diversity()
  c("shannon", "simpson", "invsimpson") %>% 
    lapply(identical_diversity, vegan::diversity) %>% 
    unlist() %>% 
    all() %>% 
    expect_true()
})

test_that("outputs equal to vegan::specnumber()", {
  diversity_wide <- vegan::specnumber(bci_wide[-1])
  diversity_long <- vgn_diversity(group_by(bci_long, rowid), n, "specnumber")
  tibble(
    long = round(diversity_long$value, 1), 
    wide = round(diversity_wide, 1)
  ) %>% 
    mutate(is_equal = identical(long, wide)) %>% 
    pull() %>% 
    all() %>% 
    expect_true()
})

test_that("Ceros don't matter", {
  with_cero <- c(1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0)
  no_cero <- c(1, 1, 1, 1, 1)
  
  # Ceros don't matter for vegan::diversity()
  .indices <- c("shannon", "simpson", "invsimpson")
  identical(
    .indices %>% map_dbl(~vegan::diversity(with_cero, .x)), 
    .indices %>% map_dbl(~vegan::diversity(no_cero, .x))
  ) %>% 
    expect_true()
  
  # Ceros don't matter for specnumber()
  out <- list(with_cero, no_cero) %>% 
    map_dbl(vegan::specnumber) %>% 
    unique() %>% 
    length() == 1
    expect_true(out)
})




