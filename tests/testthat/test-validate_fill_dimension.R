context("validate_validate_fill_dimension")

class1 <- 1:2
class2 <- letters[1:5]
fill = 0
.data <- array(
  data = c(1, 2, NA),
  dim = c(2, 4),
  dimnames = list(1:2, letters[1:4])
)

test_that("warns with .data of wrong dimension", {
  expect_error(
    expect_warning(validate_fill_dimension(c(.data), class1, class2, fill))
  )
})

test_that("class1 can't be NA but can be NULL", {
  expect_warning(
    validate_fill_dimension(.data, class1 = NA, class2 = colnames(.data))
  )
  expect_silent(validate_fill_dimension(.data, class1 = NULL, class2 = colnames(.data)))
})

test_that("class2 can't be of length shorter than columns in .data", {
  expect_error(
    validate_fill_dimension(.data, class1, class2 = colnames(.data)[1:ncol(.data) - 1])
  )
})

test_that("fill can be any numeric`", {
  expect_silent(
    validate_fill_dimension(.data, class1, class2, fill = 999)
  )
  expect_silent(
    validate_fill_dimension(.data, class1, class2, fill = -999)
  )
})
