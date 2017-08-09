context("warn_bad_arg_to_id_rowcol")

test_that("throws warnings when it should", {
  wrapper <- function(.row = 1:3,
                      .col = 4:6,
                      gridsize = 20,
                      plotdim = c(1000, 500)) {
    warn_bad_arg_to_id_rowcol(.row = .row, .col = .col, gridsize = gridsize,
      plotdim = plotdim
    )
  }
  expect_silent(wrapper())
  expect_warning(wrapper(.row = 0:2), 
    "Bad argument detected: .row <= 0", fixed = TRUE)
  expect_warning(wrapper(.col = 0:2), 
    "Bad argument detected: .col <= 0", fixed = TRUE)
  
  gridsize <- 20
  plotdim <- c(1000, 500)
  .row_too_large <- 1 + (plotdim[2] / gridsize)
  expect_warning(wrapper(.row = .row_too_large), 
    "Bad argument detected: .row > plotdim[2]/gridsize", fixed = TRUE)
  
  .col_too_large <- 1 + (plotdim[1] / gridsize)
  expect_warning(wrapper(.col = .col_too_large), 
    "Bad argument detected: .col > plotdim[1]/gridsize", fixed = TRUE)
})
