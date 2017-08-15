context("ensure_gridsize_plodim_match")

test_that("difference_among_grid_steps() returns scalar", {
  expect_true(
    assertive::is_scalar(difference_among_grid_steps(bci_habitat$x))
  )
  expect_true(
    assertive::is_scalar(difference_among_grid_steps(bci_habitat$y))
  )
})

