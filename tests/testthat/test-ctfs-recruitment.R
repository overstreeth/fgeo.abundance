context("test-recruitment.R")

# Some data to play with
census1 <- forestr::bci12t6mini
census2 <- forestr::bci12t7mini



test_that("works with predictable inputs", {
  out1 <- recruitment(census1, census2)
  expect_type(out1, "list")
  expect_length(out1, 8)
  expect_false(any(is.na(out1)))
  
  # mindbh works: output is valid and different from before
  out2 <- recruitment(census1, census2, mindbh = 50)
  expect_type(out2, "list")
  expect_length(out2, 8)
  expect_false(any(is.na(out2)))
  expect_false(identical(out1, out2))
  
  # spliting by one works
  # expect warning because some splitting groups have dbh all equal to NA
  expect_warning(out3 <- recruitment(census1, census2, split1 = census1$sp))
  expect_warning(out4 <- recruitment(census1, census2, split1 = census2$sp))
  # output is valid
  expect_type(out3, "list")
  expect_length(out3, 8)
  expect_false(any(is.na(out3)))
  # spliting by sp of census 1 or census 2 is the same
  expect_true(identical(out3, out4))
  # and the result is different than not splitting at al 
  expect_false(identical(out1, out4))
  
  # Splitting by two works
  # expect warning because some splitting groups have dbh all equal to NA
  out5 <- expect_warning(
    recruitment(census1, census2, split1 = census1$sp, split2 = census1$quadrat)
  )
  # output is valid
  # now we still get a list but each element is a double
  expect_type(out5[[1]], "double")
})
