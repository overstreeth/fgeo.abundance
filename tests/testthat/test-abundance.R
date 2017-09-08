context("abundance")

stem <- bci12s7mini

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
