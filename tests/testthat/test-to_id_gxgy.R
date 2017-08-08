context("to_id_gxgy")

library(dplyr)
mini_cns <- bci::bci12full7 %>% select(gx, gy) %>% sample_n(10)

test_that("outputs the same as ctfs::rowcol.to.index", {
  before <- ctfs::gxgy.to.index(mini_cns$gx, mini_cns$gy, gridsize = 20, 
    plotdim = c(1000, 500))
  now <- to_id_gxgy(mini_cns$gx, mini_cns$gy, gridsize = 20, 
    plotdim = c(1000, 500))
  expect_equal(before, now)
})

test_that("odd gx throws warning or error", {
  mini_cns$gx[1] <- NA_real_
  expect_warning(
    to_id_gxgy(mini_cns$gx, mini_cns$gy, gridsize = 20, plotdim = c(1000, 500))
  )
  mini_cns$gx[1] <- NaN
  expect_warning(
    to_id_gxgy(mini_cns$gx, mini_cns$gy, gridsize = 20, plotdim = c(1000, 500))
  )
  mini_cns$gx[1] <- Inf
  expect_warning(
    to_id_gxgy(mini_cns$gx, mini_cns$gy, gridsize = 20, plotdim = c(1000, 500))
  )
})

# Reset data with clean gx
mini_cns2 <- bci::bci12full7 %>% select(gx, gy) %>% sample_n(10)

test_that("odd gy throws warning or error", {
  mini_cns2$gy[1] <- NA_real_
  expect_warning(
    to_id_gxgy(mini_cns2$gx, mini_cns2$gy, gridsize = 20, 
      plotdim = c(1000, 500))
  )
  mini_cns2$gy[1] <- NaN
  expect_warning(
    to_id_gxgy(mini_cns2$gx, mini_cns2$gy, gridsize = 20, 
      plotdim = c(1000, 500))
  )
  mini_cns2$gy[1] <- Inf
  expect_warning(
    to_id_gxgy(mini_cns2$gx, mini_cns2$gy, gridsize = 20, 
      plotdim = c(1000, 500))
  )
})
