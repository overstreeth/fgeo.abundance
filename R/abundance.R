abundance <- function(x, .status = "A", ...) {
  if (missing(...)) {
    return(count_status(x = x, .status = .status))
  }
  
  count_status_by(x = x, .status = .status, ...)
}

count_status_by <- function(x, .status, ...) {
  # To later add species excluded by status, so that their count is cero
  all_sp <- unique(x["sp"])
  
  identified <- add_id(x)
  filtered <- filter_status(x = identified, .status = .status)
  grouped_summary <- dplyr::count(filtered, ...)
  # dplyr::count(filtered, sp)
  
  # to excluded species give a count of cero
  all_sp_n <- dplyr::right_join(grouped_summary, all_sp)
  tidyr::replace_na(all_sp_n, list(n = 0))
}

count_status <- function(x, .status) {
  identified <- add_id(x)
  filtered <- filter_status(x = identified, .status = .status)
  dplyr::tally(filtered)[["n"]]
}

add_id <- function(x, .status) {
  x$id <- paste0(x$treeID, "_", x$stemID)
  x
}

filter_status <- function(x, .status) {
  stopifnot(all(.status %in% unique(x$status)))
  x[x$status %in% .status, ]
}

# Tests -------------------------------------------------------------------

context("abundance")

test_that("outputs the same as ctfs::abundance() with defaults", {
  x <- bciex::bci12t7mini
  now <- abundance(x = x, .status = "A")
  before <- ctfs::abundance(x)$abund$all
  expect_equal(before, now)
})

test_that("outputs an integer", {
  expect_type(now, "integer")
})

test_that("outputs the same as ctfs::abundance with defaults", {
  now <- abundance(x = x, .status = "A", sp) %>% arrange(sp)
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
})

test_that("outputs an integer", {
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
  .status <- "invalid_status"
  expect_error(
    filter_status(x = x, .status = .status)
  )
})

