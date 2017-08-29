context("count_alive")

library(dplyr)

test_that("outputs the same as ctfs::abundance() with defaults", {
  x <- bciex::bci12t7mini
  now <- count_alive(x = x)
  before <- ctfs::abundance(x)$abund$all
  expect_equal(before, now)
})

test_that("outputs an integer", {
  x <- bciex::bci12t7mini
  now <- count_alive(x = x)
  expect_type(now, "integer")
})

test_that("outputs the same as ctfs::abundance with defaults", {
  x <- bciex::bci12t7mini
  now <- count_alive(x = x, sp) %>% arrange(sp)
  before <- x %>%
    split(.$sp) %>%
    purrr::map(ctfs::abundance) %>%
    purrr::map("abund") %>%
    tibble::enframe() %>%
    tidyr::unnest() %>% 
    rename(sp = name, n = all) %>% 
    arrange(sp)
  expect_equal(now, before)
})



context("count_status_by")

test_that("outputs the same as ctfs::abundance with defaults", {
  x <- bciex::bci12t7mini
  now <- count_status_by(x = x, .status = "A", sp) %>% arrange(sp)
  before <- x %>%
    split(.$sp) %>%
    purrr::map(ctfs::abundance) %>%
    purrr::map("abund") %>%
    tibble::enframe() %>%
    tidyr::unnest() %>% 
    rename(sp = name, n = all) %>% 
    arrange(sp)
  expect_equal(now, before)
})



context("count_status")

test_that("outputs the same as ctfs::abundance() with defaults", {
  x <- bciex::bci12t7mini
  now <- count_status(x = x, .status = "A")
  before <- ctfs::abundance(x)$abund$all
  expect_equal(before, now)
  
  now <- count_status(x = x, .status = "A", sp) %>% arrange(sp)
  before <- x %>%
    split(.$sp) %>%
    purrr::map(ctfs::abundance) %>%
    purrr::map("abund") %>%
    tibble::enframe() %>%
    tidyr::unnest() %>% 
    rename(sp = name, n = all) %>% 
    arrange(sp)
  expect_equal(now, before)
})

test_that("outputs an integer", {
  x <- bciex::bci12t7mini
  now <- count_status(x = x, .status = "A")
  expect_type(now, "integer")
})



context("tally_status")

test_that("outputs is the same as default ctfs::abundance", {
  x <- bciex::bci12t7mini
  now <- tally_status(x = x, .status = "A")
  
  before <- ctfs::abundance(x)$abund$all
  expect_equal(before, now)
  
  expect_type(now, "integer")
})

test_that("outputs an integer", {
  x <- bciex::bci12t7mini
  now <- tally_status(x = x, .status = "A")
  expect_type(now, "integer")
})



context("add_id")

test_that("adds an extra variable called id", {
  x <- bciex::bci12t7mini
  expect_equal(length(add_id(x)), length(x) + 1)
  
  expect_true(
    any(grepl("id", names(add_id(x))))
  )
})



context("filter_status")

test_that("extracts the desired statuses", {
  x <- bciex::bci12s7mini
  
  .status <- "A"
  filtered <- filter_status(x = x, .status = .status)
  expect_equal(unique(filtered$status), .status)
  
  .status <- c("A", "G")
  filtered <- filter_status(x = x, .status = .status)
  expect_equal(unique(filtered$status), .status)
  })


test_that("fails if .status is not one of x$status", {
  x <- bciex::bci12s7mini
  .status <- "invalid_status"
  expect_error(
    filter_status(x = x, .status = .status)
  )
})

