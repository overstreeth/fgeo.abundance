context("abundance")

suppressWarnings(library(dplyr))

stem <- fgeo.data::luquillo_stem_random_tiny

df <- data.frame(
  sp = rep(letters[1:3], each = 2),
  status = rep(c("A", "D"), 3),
  quadrat = 1:6
)

test_that("returns a data frame", {
  result <- abundance(stem)
  expect_type(result , "list")
  expect_true(is.data.frame(abundance(stem)))
})

test_that("Argument only_alive works as expected", {
  result <- abundance(stem, group_by = "status", only_alive = FALSE)
  expect_length(unique(result$status), 3)
  
  result <- abundance(df, only_alive = FALSE)
  expect_equal(nrow(result), 6)

  result <- abundance(df, only_alive = TRUE)
  expect_equal(nrow(result), 3)
})


test_that("group_by = NULL throws error", {
  expect_error(abundance(stem, group_by = NULL))
})



test_that("weird arguments throw error", {
  expect_error(abundance(NULL))
  expect_error(abundance(numeric(0)))
  expect_error(abundance(NA))
  
  expect_error(abundance(stem, group_by = NULL))
  
  expect_error(abundance(stem, only_alive = NULL))  

})


test_that("tricky objects in global environment cause no scoping issues", {
  # Create a confusing variable on the global environment
  # confusing because `parsed_groups` exists in the function's body
  parsed_groups <- c("status")  # this should be ignored
  nms <- abundance(df) %>% 
    as_tibble() %>% 
    names()
  expect_false("status" %in% nms)

  group_by <- c("status")  # this should be ignored
  nms <- abundance(df) %>% 
    as_tibble() %>% 
    names()
  expect_false("status" %in% nms)

  group_by <- c("status")  # this should be ignored
  nms <- abundance(df, group_by = group_by) %>% 
    as_tibble() %>% 
    names()
  expect_true("status" %in% nms)
})



context("abundance_tally")

stem <- fgeo.data::luquillo_stem_random_tiny

df <- data.frame(
  sp = rep(letters[1:3], each = 2),
  status = rep(c("A", "D"), 3),
  quadrat = 1:6
)

test_that("is integer", {
  result <- abundance_tally(stem)
  expect_type(result, "integer")
})


test_that("alive in df equals 3, all equals 6", {
  result <- abundance_tally(df)
  expect_equal(result, 3)
  
  result <- abundance_tally(df, only_alive = FALSE)
  expect_equal(result, 6)
})
