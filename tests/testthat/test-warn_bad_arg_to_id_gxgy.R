context("warn_bad_arg_to_id_gxgy")


test_that("throws warnings when it should", {
  wrapper <- function(gx = c(927.1, 22.3, 541.8),
                      gy = c(495.3, 84.8, 434.2),
                      gridsize = 20,
                      plotdim = c(1000, 500)) {
    warn_bad_arg_to_id_gxgy(gx = gx, gy = gy, gridsize = gridsize,
      plotdim = plotdim
    )
  }
  expect_silent(wrapper())
  expect_warning(wrapper(gx = c(-927.1, 22.3, 541.8)),
    "Bad argument detected: gx < 0", fixed = TRUE)
  expect_warning(wrapper(gy = c(-495.3, 84.8, 434.2)),
    "Bad argument detected: gy < 0", fixed = TRUE)
  
  
  
  plotdim <- c(1000, 500)
  gx_too_large <- plotdim[1]
  expect_warning(wrapper(gx = gx_too_large),
    "Bad argument detected: gx >= plotdim[1]", fixed = TRUE)
  
  plotdim <- c(1000, 500)
  gy_too_large <- plotdim[2]
  expect_warning(wrapper(gy = gy_too_large),
    "Bad argument detected: gy >= plotdim[2]", fixed = TRUE)
  
  expect_warning(wrapper(gx = c(927.1, 22.3, NA)),
    "Bad argument detected: is.na(gx)", fixed = TRUE)
  
  
  
  
})
