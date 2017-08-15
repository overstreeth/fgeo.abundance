context("abundance")

library(dplyr)
cns5_mini <- sample_n(bci::bci12full5, 100)

test_that("outputs the same as ctfs::abundance()", {
  old <- ctfs::abundance(cns5_mini)
  new <- forestr::abundance(cns5_mini)
  expect_equal(old, new)
  
  old <- ctfs::abundance(cns5_mini, mindbh = 10)
  new <- forestr::abundance(cns5_mini, mindbh = 10)
  expect_equal(old, new)

  old <- ctfs::abundance(
    cns5_mini,
    type = "ba",
    mindbh = 10,
    split1 = cns5_mini$sp
  )
  new <- forestr::abundance(
    cns5_mini,
    type = "ba",
    mindbh = 10,
    split1 = cns5_mini$sp
  )
  expect_equal(old, new)

  old <- ctfs::abundance(
    cns5_mini,
    type = "agb",
    mindbh = 10,
    split1 = cns5_mini$sp
  )
  new <- forestr::abundance(
    cns5_mini,
    type = "agb",
    mindbh = 10,
    split1 = cns5_mini$sp
  )
  expect_equal(old, new)
})
