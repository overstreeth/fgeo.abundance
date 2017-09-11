context("basal_area_ind")

df <- data.frame(
  sp = rep(letters[1:3], each = 2),
  status = rep(c("A", "D"), 3),
  quadrat = 1:6,
  dbh = rnorm(6)
)

test_that("retuns a numeric vector", {
  result <- suppressMessages(basal_area_ind(df$dbh))
  expect_type(result, "double")
  expect_true(purrr::is_vector(result))
})
