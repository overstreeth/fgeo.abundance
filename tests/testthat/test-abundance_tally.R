context("abundance_tally")

stem <- bci12s7mini

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
