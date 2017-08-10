context("warn_any_odd_arguments")

my_argument <- c(0:10, NA)
my_tests <- list(
  quote(my_argument <= 0),
  quote(is.na(my_argument)),
  quote(my_argument == 0)
  )

test_that("throws expected warning", {
  expect_warning(
    warn_any_odd_arguments(x = my_argument, quoted_tests_list = my_tests),
    "Some my_argument <= 0", fixed = TRUE)
  expect_warning(
    warn_any_odd_arguments(x = my_argument, quoted_tests_list = my_tests),
    "Some is.na(my_argument)", fixed = TRUE)
  expect_warning(
    warn_any_odd_arguments(x = my_argument, quoted_tests_list = my_tests,
      msg = "watch out!! "),
    "watch out!! my_argument == 0", fixed = TRUE)
})
