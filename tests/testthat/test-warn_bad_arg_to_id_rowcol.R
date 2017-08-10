context("warn_bad_arg_to_id_rowcol")

test_that("warn_bad_arg_to_id_rowcol() warns with bad arguments", {
  expect_warning(
    warn_bad_arg_to_id_rowcol(
      .row = NA,
      .col = -2:3,
      gridsize = 20,
      plotdim = c(1000, 500)
    )
  )
  expect_warning(
    warn_bad_arg_to_id_rowcol(
      .row = (500 / 20) + 1,
      .col = 2,
      gridsize = 20,
      plotdim = c(1000, 500)
    ),
    ".row > plotdim[2]/gridsize", fixed = TRUE
  )
})
test_that("warn_bad_arg_to_id_rowcol() is silent with good arguments", {
  expect_silent(
    warn_bad_arg_to_id_rowcol(
      .row = 1:3,
      .col = 1:3,
      gridsize = 20,
      plotdim = c(1000, 500)
    )
  )
  expect_silent(
    warn_bad_arg_to_id_rowcol(
      .row = 2,
      .col = 2,
      gridsize = 0,
      plotdim = c(0, 500)
    )
  )
})
