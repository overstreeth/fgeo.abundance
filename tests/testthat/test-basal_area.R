context("basal_area")

library(tidyverse)

df <- data.frame(
  sp = rep(letters[1:3], each = 2),
  status = rep(c("A", "D"), 3),
  quadrat = 1:6,
  dbh = rnorm(6)
)

test_that("returns a data frame", {
  result <- suppressMessages(basal_area(df))
  expect_type(result , "list")
  expect_true(is.data.frame(basal_area(df)))
})

test_that("returns the same as *_ind if data has only one row.", {
  df1 <- df[1, ] %>%
    dplyr::group_by(status)
  expected <- suppressMessages(basal_area_ind(df1$dbh))
  actual <- suppressMessages(basal_area(df1)$basal_area)
  expect_equal(actual, expected)
})


test_that("returns the correct sum", {
  df$ba <- suppressMessages(basal_area_ind(df$dbh))
  
  actual <- sum(basal_area(df, "quadrat", FALSE) %>% pull(basal_area))
  expected <- sum(df$ba)
  expect_equal(actual, expected)
  expect_true(near(round(actual, 3), round(4.712389, 3)))
})

test_that("Argument only_alive works as expected", {
  result <- suppressMessages(
    basal_area(df, group_by = "status", only_alive = FALSE)
  )
  expect_length(unique(result$status), 2)
  
  # Using default group_by = c("quadrat", "sp")
  result <- suppressMessages(basal_area(df, only_alive = FALSE))
  expect_equal(nrow(result), 6)

  result <- suppressMessages(basal_area(df, only_alive = TRUE))
  expect_equal(nrow(result), 3)
})


test_that("group_by = NULL throws error", {
  expect_error(basal_area(df, group_by = NULL))
})



test_that("weird arguments throw error", {
  expect_error(basal_area(NULL))
  expect_error(basal_area(numeric(0)))
  expect_error(basal_area(NA))
  
  expect_error(basal_area(df, group_by = NULL))
  
  expect_error(basal_area(df, only_alive = NULL))  

})

test_that("tricky objects in global environment cause no scoping issues", {
  # Create a confusing variable on the global environment
  # confusing because `parsed_groups` exists in the function's body
  parsed_groups <- c("status")  # this should be ignored
  nms <- basal_area(df) %>% 
    as_tibble() %>% 
    names()
  expect_false("status" %in% nms)

  group_by <- c("status")  # this should be ignored
  nms <- basal_area(df) %>% 
    as_tibble() %>% 
    names()
  expect_false("status" %in% nms)

  group_by <- c("status")  # this should be ignored
  nms <- basal_area(df, group_by = group_by) %>% 
    as_tibble() %>% 
    names()
  expect_true("status" %in% nms)
})










