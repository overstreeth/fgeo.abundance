## ----setup, echo = FALSE, message=FALSE, warning=FALSE-------------------
# hadley's settings
set.seed(1014)
options(digits = 3)

knitr::opts_chunk$set(
  echo = TRUE,  # {mine}
  comment = "#>",
  collapse = TRUE,
  # cache = TRUE,
  out.width = "70%",
  fig.align = "center",
  fig.width = 6,
  fig.asp = 0.618,  # 1 / phi
  fig.show = "hold"
)

options(dplyr.print_min = 6, dplyr.print_max = 6)

## ------------------------------------------------------------------------
# To install from a private repo, generate a personal access token (PAT) in
# https://github.com/settings/tokens and supply to this argument.
 GITHUB_PAT <- "your token"
# install_github("forestgeo/forestr@iss49_demography", auth_token = GITHUB_PAT)
library(forestr)

# Also using the package tibble for nicer printing
library(tibble)
# Print no more than 10 rows to save space and time
options(tibble.print_max = 10)

## ---- error=TRUE---------------------------------------------------------
# Some data to play with
census1 <- forestr::bci12t6mini
census2 <- forestr::bci12t7mini



# Using the new wrapper `<FUNCTION>_df()` ---------------------------------

# A REALISTIC CASE

# Note `recruitment_df()` warns if some groups have dbh values full of NA
split_by_sp <- recruitment_df(census1, census2, split1 = census1$sp)
split_by_sp

# Same for `mortality_df()`; let's explore a few species
# We don't need to be warned again (suppressing warnings)
all_species <- suppressWarnings(
  mortality_df(census1, census2, split1 = census1$sp)
)
few_species <- sample(unique(all_species$split), 5)
long_format <- subset(all_species, split %in% few_species)
long_format

# From long to wide format
library(tidyr)

wide_format <- tidyr::spread(long_format, key = split, value = value)
wide_format

# THE SIMPLEST CASE

# split1 defaults to `NULL`; output keeps the variable `split` for consistency
split_null <- recruitment_df(census1, census2)
split_null

# For this simple case, you may want to use this alternative approach
result <- recruitment(census1, census2)
result_df <- as.data.frame(unlist(result))
head(result_df, 10)

# But that approach doesn't help with splitting variables because we loose 
# information about what matric each value belongs to
result <- recruitment(census1, census2, split1 = census1$sp)
result_df <- as.data.frame(unlist(result))
head(result_df, 10)

# Using the traditional functions -----------------------------------------

# The output is more ackward to explore
traditional_by_sp <- suppressWarnings(
  recruitment(census1, census2, split1 = census1$sp)
)
str(traditional_by_sp)

# In RStudio 1.1 the function `View()` will help you see ackward lists.
# Here showing a simple trick
lapply(traditional_by_sp, head)

# `recruitment()` can take up to two splitting variables; not `recruitment_df()`
traditional_by_sp_and_quadrat <- suppressWarnings(
  recruitment(census1, census2, split1 = census1$sp, split2 = census1$quadrat)
)
str(traditional_by_sp_and_quadrat)

# This intentionally fails -- to keep the output easy to view and manipulate
recruitment_df(census1, census2, split1 = census1$sp, split2 = census1$quadrat)

# Warnings and errors -----------------------------------------------------

# Notice that the original `recruitment()` from ctfs gives no warnings
# No warning
with_ctfs <- ctfs::recruitment(census1, census2, split1 = census1$sp)
# Warning
with_forestr <- forestr::recruitment(census1, census2, split1 = census1$sp)

# What happens if input is wrong?

# Introducing a wrong name for an important variable
wrong_nm1 <- census1
wrong_nm2 <- census2
wrong_nm1$DBH <- wrong_nm1$dbh
wrong_nm1$dbh <- NULL
wrong_nm2$DBH <- wrong_nm2$dbh
wrong_nm2$dbh <- NULL

# Uninformative error (gets picked up too far into the function)
with_ctfs <- ctfs::recruitment(wrong_nm1, wrong_nm2, split1 = wrong_nm1$sp)

# More informative error (gets picket up at the top of the function)
with_forestr <- forestr::recruitment(wrong_nm1, wrong_nm2, split1 = wrong_nm1$sp)

## ---- error=TRUE---------------------------------------------------------
# Using the same data as above.
# You can expect the same warnings about `dbh` eualt to NA within some groups
# defined by `split1 = census1$sp` and `split2 = census1$quadrat`
census1 <- forestr::bci12t6mini
census2 <- forestr::bci12t7mini

# Using the new wrapper `<FUNCTION>_df()` ---------------------------------

# A REALISTIC CASE

# Note `growth_df()` warns if some groups have dbh values full of NA
split_by_sp <- growth_df(census1, census2, split1 = census1$sp)
split_by_sp

# Same for `growth_df()`; let's explore a few species
# We don't need to be warned again (suppressing warnings)
all_species <- suppressWarnings(
  growth_df(census1, census2, split1 = census1$sp)
)
few_species <- sample(unique(all_species$split), 5)
long_format <- subset(all_species, split %in% few_species)
long_format

# From long to wide format
library(tidyr)
wide_format <- tidyr::spread(long_format, key = split, value = value)
wide_format

# THE SIMPLEST CASE

# split1 defaults to `NULL`; output keeps the variable `split` for consistency
split_null <- growth_df(census1, census2)
split_null

# Using the traditional functions -----------------------------------------

# The output is more ackward to explore
traditional_by_sp <- suppressWarnings(
  growth(census1, census2, split1 = census1$sp)
)
str(traditional_by_sp)

# In RStudio 1.1 the function `View()` will help you see ackward lists.
# Here showing a simple trick
lapply(traditional_by_sp, head)

# `growth()` can take up to two splitting variables; not `growth_df()`
traditional_by_sp_and_quadrat <- suppressWarnings(
  growth(census1, census2, split1 = census1$sp, split2 = census1$quadrat)
)
str(traditional_by_sp_and_quadrat)

# This intentionally fails -- to keep the output easy to view and manipulate
growth_df(census1, census2, split1 = census1$sp, split2 = census1$quadrat)

# Warnings and errors -----------------------------------------------------

# Notice that the original `growth()` from ctfs gives no warnings
# No warning
with_ctfs <- ctfs::growth(census1, census2, split1 = census1$sp)
# Warning
with_forestr <- forestr::growth(census1, census2, split1 = census1$sp)

# What happens if input is wrong?

# Introducing a wrong name for an important variable
wrong_nm1 <- census1
wrong_nm2 <- census2
wrong_nm1$DBH <- wrong_nm1$dbh
wrong_nm1$dbh <- NULL
wrong_nm2$DBH <- wrong_nm2$dbh
wrong_nm2$dbh <- NULL

# Uninformative error (gets picked up too far into the function)
with_ctfs <- ctfs::growth(wrong_nm1, wrong_nm2, split1 = wrong_nm1$sp)

# More informative error (gets picket up at the top of the function)
with_forestr <- forestr::growth(wrong_nm1, wrong_nm2, split1 = wrong_nm1$sp)

# Some arguments exclusive of `growth()` ----------------------------------

# Here are exclusive arguments of growth; read about the others at `?growth()`

# Calculate not annual growth rate but relative growth rate
# Unlisting to fit all metrics in one line and save space
unlist(growth(census1, census2, method = "E"))

# Return `sd` instead of `clim`
unlist(growth(census1, census2, stdev = TRUE))

# Include all living trees
unlist(growth(census1, census2, mindbh = NULL))

# Measure growth not based on `dbh` but on `agb`
unlist(growth(census1, census2, growthcol = "agb"))

## ---- echo=FALSE, message=FALSE------------------------------------------
library(DependenciesGraphs)
library(handy)
library(ctfs)

## ---- echo=FALSE---------------------------------------------------------
explore_dependencies("ctfs", "recruitment")

## ---- echo=FALSE---------------------------------------------------------
explore_dependencies("ctfs", "mortality")

## ---- echo=FALSE---------------------------------------------------------
explore_dependencies("ctfs", "growth")

