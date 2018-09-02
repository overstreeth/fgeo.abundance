context("basal_area")

library(dplyr)

df <- data.frame(
  sp = rep(letters[1:3], each = 2),
  status = rep(c("A", "D"), 3),
  quadrat = 1:6,
  dbh = rnorm(6)
)

test_that("retuns a numeric vector", {
  result <- basal_area_dbl(df$dbh)
  expect_type(result, "double")
  expect_true(rlang::is_vector(result))
})

test_that("returns the expected data structure", {
  result <- basal_area(df)
  expect_type(result, "list")
  expect_true(is.data.frame(basal_area(df)))

  expect_type(basal_area_dbl(df$dbh), "double")
})

test_that("returns the correct sum", {
  df <- data.frame(
    sp = rep(letters[1:3], each = 2),
    status = rep(c("A", "D"), 3),
    quadrat = 1:6,
    dbh = rnorm(6)
  )
  df$ba <- basal_area_dbl(df$dbh)

  actual <- df %>%
    group_by(quadrat) %>%
    basal_area() %>%
    pull(basal_area) %>%
    sum()

  expected <- sum(df$ba)
  expect_equal(actual, expected)
})

test_that("weird arguments throw error", {
  expect_error(basal_area(NULL))
  expect_error(basal_area(NA))
})

test_that("tricky objects in global environment cause no scoping issues", {
  group_by <- c("status") # this should be ignored
  nms <- basal_area(df) %>%
    as_tibble() %>%
    names()
  expect_false("status" %in% nms)
})



context("add_basal_area")

stem <- tibble::tibble(
  stemID = 1:4,
  quadrat = paste0("000", rep(1:2, each = 2)),

  sp = c("sp1", "sp2", "sp1", "sp1"),
  dbh = c(1, 1, 2, 2)
)

test_that("returns similar to basal_area(group_by()) but all rows", {
  expect_equal(
    unique(basal_area(group_by(stem, quadrat))$basal_area),
    unique(add_basal_area(stem, quadrat)$basal_area)
  )
  expect_equal(nrow(basal_area(group_by(stem, quadrat))), 2)
  expect_equal(nrow(add_basal_area(stem, quadrat)), 4)
})

test_that("returns dataframe with expected structure", {
  expect_is(add_basal_area(stem, quadrat), "data.frame")
  expect_named(add_basal_area(stem, quadrat), c(names(stem), "basal_area"))
})

test_that("deals with grouping", {
  # outputs grouping as it was in input
  # ungrouped -> ungrouped
  expect_false(is_grouped_df(add_basal_area(stem)))
  # grouped -> grouped
  expect_true(is_grouped_df(add_basal_area(group_by(stem, quadrat))))
})
