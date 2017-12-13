---
title: "Abundance and Basal Area"
author: "Mauro Lepore"
date: "2017-10-25"
output:
  rmarkdown::html_vignette:
    toc: true
    toc_depth: 6
vignette: >
  %\VignetteIndexEntry{Abundance and Basal Area}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
---



# Overview

This vignette shows you how to calculate abundance and basal area with the functions `abundance()`, `basal_area()`, and friends from the __forestr__ package -- let's load it now:


```r
# To install from a private repo, generate a personal access token (PAT) in
# https://github.com/settings/tokens and supply to this argument.
 GITHUB_PAT <- "your token"
# install_github("forestgeo/forestr@iss49_demography", auth_token = GITHUB_PAT)
library(forestr)
```

# Abundance

The function `abundance()` provides a quick way to calculate the abundance of alive stems of each species in each quadrat -- that is what it does by default (see `abundance()`):


```r
stem <- bciex::bci12s7mini

n <- abundance(stem)
head(n)
#>   quadrat     sp n
#> 1    0000 soroaf 1
#> 2    0002 hybapr 1
#> 3    0006 hybapr 2
#> 4    0009 faraoc 1
#> 5    0009 hybapr 1
#> 6    0020 tet2pa 1

# Of course you can change the defaults
head(abundance(stem, "sp", only_alive = FALSE))
#>       sp  n
#> 1 acaldi 27
#> 2 aegipa  1
#> 3 alchco 11
#> 4 alibed  3
#> 5 allops  1
#> 6 alsebl 36
```

So you can get consistent output, `abundance()` always returns a dataframe. If what you want is a single number giving the total abundance of alive stems, then you can use `abundance_tally()`.


```r
abundance_tally(stem)
#> [1] 665
abundance_tally(stem, only_alive = FALSE)
#> [1] 2165

# This won't work
fails <- abundance(stem, group_by = NULL)
#> Error in group(x = x, group_by = group_by, only_alive = only_alive): group_by can't be NULL.
```

In R there is always more than one way to get the same result. An alternative is to use __dplyr__, and instead of passing _"quoted grouping variables"__, you can pass __bare grouping variables__. No approach is good or bad -- the most suitable one depends on the situation


```r
# With forestr you can assign a string of variable names and pass use it later
quoted_vars <- c("quadrat", "sp")
with_forestr <- abundance(stem, quoted_vars)

# Compare
library(dplyr)
alive <- dplyr::filter(stem, status == "A")
# With dplyr you use bare variable names
with_dplyr <- dplyr::count(alive, quadrat, sp)

# You cant reuse grouping variables saved as strings
# (other tricks work but are beyond the scope of this tutorial)
fails <- dplyr::count(alive, quoted_vars)
#> Error in grouped_df_impl(data, unname(vars), drop): Column `quoted_vars` is unknown
# This also fails, not but trowing an error but by giving the wrong output
also_fails <- dplyr::count(alive, "quadrat", "sp")

# Whatever you choose, if you do it right you get the same results
alternatives <- list(with_forestr, with_dplyr)
lapply(alternatives, head)
#> [[1]]
#>   quadrat     sp n
#> 1    0000 soroaf 1
#> 2    0002 hybapr 1
#> 3    0006 hybapr 2
#> 4    0009 faraoc 1
#> 5    0009 hybapr 1
#> 6    0020 tet2pa 1
#> 
#> [[2]]
#> # A tibble: 6 x 3
#>   quadrat     sp     n
#>     <chr>  <chr> <int>
#> 1    0000 soroaf     1
#> 2    0002 hybapr     1
#> 3    0006 hybapr     2
#> 4    0009 faraoc     1
#> 5    0009 hybapr     1
#> 6    0020 tet2pa     1

is_var_identical <- function(x, y, var) {identical(x[["var"]], y[["var"]])}
is_var_identical(with_forestr, with_dplyr, "quadrat")
#> [1] TRUE
is_var_identical(with_forestr, with_dplyr, "sp")
#> [1] TRUE
is_var_identical(with_forestr, with_dplyr, "n")
#> [1] TRUE
```


# Basal Area

Compared to `abundance()`, `basal_area()` behaves very similarly: it provides a quick way to calculate the basal area of alive trees of each species within each quadrat:


```r
stem <- bciex::bci12s7mini
ba <- basal_area(stem)
#> Units of returned values are the square of the input units.
head(ba)
#>   quadrat     sp basal_area
#> 1    0000 soroaf       2642
#> 2    0002 hybapr         95
#> 3    0006 hybapr        475
#> 4    0009 faraoc       1810
#> 5    0009 hybapr          0
#> 6    0020 tet2pa       3117

# Same but silent
silent <- suppressMessages(basal_area(stem))
head(silent)
#>   quadrat     sp basal_area
#> 1    0000 soroaf       2642
#> 2    0002 hybapr         95
#> 3    0006 hybapr        475
#> 4    0009 faraoc       1810
#> 5    0009 hybapr          0
#> 6    0020 tet2pa       3117
```

Instead if you want the basal area of each individual stem, use `basal_area_ind()`:


```r
ba <- basal_area_ind(stem$dbh)
#> Units of returned values are the square of the input units.
head(ba)
#> [1] 407150     NA     NA     NA     NA     NA

# Why NAs?
head(stem$dbh)
#> [1] 720  NA  NA  NA  NA  NA
# because `dbh` is na in the data set
```

