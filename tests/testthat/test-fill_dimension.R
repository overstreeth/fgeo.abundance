context("fill_dimension")

class1 <- 1:2
class2 <- letters[1:5]
fill = 0
.data <- array(
  data = c(1, 2, NA),
  dim = c(2, 4),
  dimnames = list(1:2, letters[1:4])
)

test_that("outputs the same as `ctfs::ctfs::fill.dimension()`", {
  expect_equal(
    ctfs::fill.dimension(.data, class1, class2, fill),
    fill_dimension(.data, class1, class2, fill)
  )
})
test_that("classes matrix, array and data.frame all work", {
  expect_equal(
    fill_dimension(as.array(.data), class1, class2, fill = 0),
    fill_dimension(as.data.frame(.data), class1, class2, fill = 0)
  )
  expect_equal(
    fill_dimension(as.array(.data), class1, class2, fill = 0),
    fill_dimension(as.matrix(.data), class1, class2, fill = 0)
  )
})
