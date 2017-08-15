context("abundance_match_census_habitat")

test_that("value is a data frame with subset equal to stored reference", {

  # ctf::abundanceperquad() outputs the same as forestr::abundanceperquad() but
  # only forestr::abundanceperquad throws warnings
  expect_warning(
    in_forestr <- forestr::abundanceperquad(
      bci_mini, plotdim = c(1000, 500), gridsize = 20
    )$abund
  )
  # No warnings here
  in_ctfs <- ctfs::abundanceperquad(
    bci_mini, plotdim = c(1000, 500), gridsize = 20
  )$abund
  expect_equal(in_forestr, in_ctfs)

  # Also outputs the same, but simpler (less arguments) and safer (forces match
  # in gridsize and plotdim between census data and habitat data)
  expect_warning(
    in_forestr_forcing_match <- abundance_match_census_habitat(
      bci_mini, bci_habitat
    )
  )
  expect_equal(in_forestr, in_forestr_forcing_match)
})
