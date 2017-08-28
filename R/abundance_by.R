# Filters alive and adds unique id
id_alive <- function(x) {
  alive <- x[x$status == "A", ]
  alive$id <- paste0(alive$treeID, "_", alive$stemID)
  alive
}

context("id_alive")
test_that("has the extra variable id", {
  x <- bci12t7mini
  expect_equal(length(id_alive(x)), length(x) + 1)
  
  expect_true(
    any(grepl(
      "id", names(id_alive(x))
    ))
  )
})



count_alive <- function(x) {
  alive_id <- id_alive(x)
  length(unique(alive_id$id))
}

library(testthat)
context("count_alive")

test_that("outputs the same as ctfs::abundance when type = 'abund'.", {
  # Tree data
  now <- count_alive(bci12t1mini)
  before <- unname(ctfs::abundance(bci12t1mini)$abund$all)
  expect_equal(before, now)
  # Stem data
  now <- count_alive(bci12s1mini)
  before <- unname(ctfs::abundance(bci12s1mini)$abund$all)
  expect_equal(before, now)
})



count_alive_by <- function(x, ...) {
  all_sp <- unique(x["sp"])
  alive <- id_alive(x)
  count_alive <- dplyr::count(alive, ...)
  count_all_sp <- dplyr::right_join(count_alive, all_sp)
  tidyr::replace_na(count_all_sp, list(n = 0))
}

context("count_alive_by")

test_that("outputs the same as ctfs::abundance with defaults", {
  x <- bci12t7mini
  now <- count_alive_by(x, sp) %>% arrange(sp)
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








