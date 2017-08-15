context("extract_plot_dimensions")

test_that("with habitats = bci_habitat gridsize value is 20", {
  expect_equal(extract_gridsize(bci_habitat), 20)
  expect_equal(extract_gridsize(bci_habitat), 20)
})
test_that("with habitats = bci_habitat plotdim value is c(1000,  500)", {
  expect_equal(extract_plotdim(bci_habitat)[[1]], 1000)
  expect_equal(extract_plotdim(bci_habitat)[[2]], 500)
})
test_that("value are of correct type", {
  expect_type(extract_gridsize(bci_habitat), "integer")
  expect_type(extract_plotdim(bci_habitat), "integer")
})
test_that("Output of extract_plotdim is unnamed", {
  expect_null(names(extract_plotdim(bci::bci_habitat)))
})
