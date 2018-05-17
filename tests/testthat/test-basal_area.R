context("basal_area")

library(dplyr)

df <- data.frame(
  sp = rep(letters[1:3], each = 2),
  status = rep(c("A", "D"), 3),
  quadrat = 1:6,
  dbh = rnorm(6)
)

test_that("retuns a numeric vector", {
  result <- basal_area(df$dbh)
  expect_type(result, "double")
  expect_true(purrr::is_vector(result))
})

test_that("returns the expected data structure", {
  result <- basal_area(df)
  expect_type(result, "list")
  expect_true(is.data.frame(basal_area(df)))
  
  expect_type(basal_area(df$dbh), "double")
})

test_that("returns the same as basal_area.numeric if data has only one row.", {
  df1 <- df[1, ] %>%
    dplyr::group_by(status)
  expected <- basal_area(df1$dbh)
  actual <-   basal_area(df1)$basal_area
  expect_equal(actual, expected)
})

test_that("returns the correct sum", {
  df <- data.frame(
    sp = rep(letters[1:3], each = 2),
    status = rep(c("A", "D"), 3),
    quadrat = 1:6,
    dbh = rnorm(6)
  )
  df$ba <- basal_area(df$dbh)
  
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

