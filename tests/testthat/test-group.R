context("group")

stem <- bciex::bci12s7mini

library(dplyr)
library(tibble)

stem <- bciex::bci12s7mini

df <- data.frame(
  sp = rep(letters[1:3], each = 2),
  status = rep(c("A", "D"), 3),
  quadrat = 1:6
)

test_that("returns a grouped tibble", {
  result <- group(stem, "sp", only_alive = TRUE)
  expect_true("grouped_df" %in% class(result))
  expect_true(is.tibble(result))
})

test_that("Argument only_alive works as expected", {
  result <- group(df, group_by = "status", only_alive = FALSE)
  expect_equal(nrow(result), 6)

  result <- group(df, group_by = "sp", only_alive = TRUE)
  expect_equal(nrow(result), 3)
})

test_that("group_by = NULL throws error", {
  expect_error(group(stem, group_by = NULL, only_alive = TRUE))
})

test_that("weird arguments throw error", {
  expect_error(group(NULL, group_by = "status", only_alive = TRUE))
  expect_error(group(numeric(0), group_by = "status", only_alive = TRUE))
  expect_error(group(NA, group_by = "status", only_alive = TRUE))
  
  expect_error(group(df, group_by = NULL, only_alive = TRUE))
  
  expect_error(group(df, group_by = "status", only_alive = NULL))  
})

test_that("tricky objects in global environment cause no scoping issues", {
  # Create a confusing variable on the global environment
  # confusing because `parsed_groups` exists in the function's body
  parsed_groups <- c("status")  # this should be ignored
  res <- group(df, group_by = c("quadrat", "sp"), only_alive = TRUE) %>% 
    as_tibble()
  expect_false("status" %in% groups(res))

  group_by <- c("status")  # this should be ignored
  res <- group(df, group_by = c("quadrat", "sp"), only_alive = TRUE) %>% 
  as_tibble()
  expect_false("status" %in% groups(res))
  
  group_by <- c("status")  # this should be ignored
  res <- group(df, group_by = group_by, only_alive = TRUE) %>% 
  as_tibble()
  expect_true("status" %in% groups(res))
})
