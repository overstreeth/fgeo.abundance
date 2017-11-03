context("test-mortality.R")

library(dplyr)

# Some data to play with
census1 <- forestr::bci12t6mini
census2 <- forestr::bci12t7mini
# 10x smaller data to save time
tiny1 <- dplyr::filter(census1, sp  %in% unique(census1$sp)[1:10])
tiny2 <- dplyr::filter(census2, sp  %in% unique(census1$sp)[1:10])
# Check the sp filtered for each data set are the same
expect_equal(
  tiny1$sp %>% unique, 
  tiny2$sp %>% unique
)



# mortality ---------------------------------------------------------------

# Test new data with function that knownly works
test_that("recruitment works with tiny data", {
  expect_type(recruitment(tiny1, tiny2), "list")
})

# Now continue testing mortality -- the function relevant here

test_that("works with predictable inputs", {
  out1 <- mortality(tiny1, tiny2)
  expect_type(out1, "list")
  expect_length(out1, 9)
  expect_false(any(is.na(out1)))
  
  # out2 was tested mindbh, which exista in recruitment() but not in mortality()
  
  # spliting by one works
  # expect warning because tiny2 has two species which dbh is all NA
  out3 <- expect_warning(mortality(tiny1, tiny2, split1 = tiny1$sp))
  out4 <- expect_warning(mortality(tiny1, tiny2, split1 = tiny2$sp))

  # output is valid
  expect_type(out3, "list")
  expect_length(out3, 9)
  expect_false(any(is.na(out3)))
  # spliting by sp of census 1 or census 2 is the same
  expect_true(identical(out3, out4))
  # and the result is different than not splitting at al 
  expect_false(identical(out1, out4))
  
  # Splitting by two works
  out5 <- expect_warning(
    mortality(tiny1, tiny2, split1 = tiny1$sp, split2 = tiny1$quadrat)
  )
  # output is valid
  # now we still get a list but each element is a double
  expect_type(out5[[1]], "double")
})

test_that("missing crucial names throws error", {
  crucial_nms <- c("dbh", "pom", "status", "date")

  newnm1 <- dplyr::rename(tiny1, xxx = dbh)
  newnm2 <- dplyr::rename(tiny2, xxx = dbh)
  expect_error(mortality(newnm1, newnm2))

  newnm1 <- dplyr::rename(tiny1, xxx = pom)
  newnm2 <- dplyr::rename(tiny2, xxx = pom)
  expect_error(mortality(newnm1, newnm2))

  newnm1 <- dplyr::rename(tiny1, xxx = status)
  newnm2 <- dplyr::rename(tiny2, xxx = status)
  expect_error(mortality(newnm1, newnm2))

  newnm1 <- dplyr::rename(tiny1, xxx = date)
  newnm2 <- dplyr::rename(tiny2, xxx = date)
  expect_error(mortality(newnm1, newnm2))
})



# mortality_df() -------------------------------------------------------------

test_that("works with predictable inputs", {
  # No splitting variable
  out1 <- mortality_df(tiny1, tiny2)
  expect_is(out1, "data.frame")
  expect_length(out1, 3)
  
  # One splitting variable
  # skipping out2
  # expect warning because tiny2 has two species which dbh is all NA
  expect_warning(out3 <- mortality_df(tiny1, tiny2, split1 = tiny1$sp))
  # output has correct structure
  expect_is(out3, "data.frame")
  expect_length(out3, 3)

  # Two splitting variables fail because split2 is not defined in mortality_df
  expect_error(
    mortality_df(tiny1, tiny2, split1 = tiny1$sp, split2 = tiny1$quadrat)
  )
})
