
<!-- README.md is generated from README.Rmd. Please edit that file -->

# <img src="https://i.imgur.com/vTLlhbp.png" align="right" height=88 /> Abundance, basal area and diversity

[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![Travis build
status](https://travis-ci.org/forestgeo/fgeo.abundance.svg?branch=master)](https://travis-ci.org/forestgeo/fgeo.abundance)
[![Coverage
status](https://codecov.io/gh/forestgeo/fgeo.abundance/branch/master/graph/badge.svg)](https://codecov.io/github/forestgeo/fgeo.abundance?branch=master)
[![CRAN
status](http://www.r-pkg.org/badges/version/fgeo.abundance)](https://cran.r-project.org/package=fgeo.abundance)

**fgeo.abundance** calculates abundance, basal area and diversity.

## Installation

[Install all **fgeo** packages in one
step](https://forestgeo.github.io/fgeo/index.html#installation)

``` r
# install.packages("remotes")
remotes::install_github(repo = "forestgeo/fgeo.abundance")
```

For details on how to install packages from GitHub, see [this
article](https://goo.gl/dQKEeg).

## Example

``` r
library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
library(fgeo.abundance)

# Example data.
census <- tibble(
  treeID = c(1, 1, 2, 3, 3, 3),
  stemID = c(1, 2, 3, 4, 5, 6),
  quadrat = paste0("000", rep(1:2, each = 3)),
  sp = c(paste0("sp", c(1, 1, 2)), paste0("sp", c(3, 3, 3))),
  dbh = abs(sample(rnorm(100), 6) * 10)
)
census
#> # A tibble: 6 x 5
#>   treeID stemID quadrat sp      dbh
#>    <dbl>  <dbl> <chr>   <chr> <dbl>
#> 1      1      1 0001    sp1   20.3 
#> 2      1      2 0001    sp1    3.40
#> 3      2      3 0001    sp2    8.32
#> 4      3      4 0002    sp3   14.4 
#> 5      3      5 0002    sp3    4.02
#> 6      3      6 0002    sp3    2.37

# `count_distinct() ` is general: works with any data and has few restrictions.
count_distinct(census, treeID)
#> # A tibble: 1 x 1
#>       n
#>   <int>
#> 1     3

by_quad <- group_by(census, quadrat)
count_distinct(by_quad, stemID)
#> # A tibble: 2 x 2
#>   quadrat     n
#>   <chr>   <int>
#> 1 0001        3
#> 2 0002        3

# `count_distinct_stemid()` is specialized.
count_distinct_stemid(by_quad)
#> # A tibble: 2 x 2
#>   quadrat     n
#>   <chr>   <int>
#> 1 0001        3
#> 2 0002        3

# `count_distinct_treeid()` is also specialized and has some safety features.
# If treeID is duplicated, counting distinct treeid is an (intentional) error.
count_distinct_treeid(by_quad)
#> Error:   Detected duplicated values of treeid.
#>     Expected unique values of treeid within each data-group (if any)

# First, collapse treeID by picking one stem, e.g. of maximum dbh.
largest_stem <- fgeo.tool::by_treeid_pick_dbh_max(census)
largest_stem
#> # A tibble: 3 x 5
#>   treeID stemID quadrat sp      dbh
#>    <dbl>  <dbl> <chr>   <chr> <dbl>
#> 1      1      1 0001    sp1   20.3 
#> 2      3      4 0002    sp3   14.4 
#> 3      2      3 0001    sp2    8.32

# Then count treeIDs
count_distinct_treeid(largest_stem)
#> # A tibble: 1 x 1
#>       n
#>   <int>
#> 1     3

# It's not an error if duplicated treeIDs belong to different groups of data.
vft <- tibble::tibble(CensusID = c(1, 1, 2, 2), TreeID = c(1, 2, 1, 2))
vft
#> # A tibble: 4 x 2
#>   CensusID TreeID
#>      <dbl>  <dbl>
#> 1        1      1
#> 2        1      2
#> 3        2      1
#> 4        2      2

by_censusid <- group_by(vft, CensusID)
count_distinct_treeid(by_censusid)
#> # A tibble: 2 x 2
#>   CensusID     n
#>      <dbl> <int>
#> 1        1     2
#> 2        2     2
```

[Get started](https://forestgeo.github.io/fgeo/articles/fgeo.html)

## Information

  - [Getting help](SUPPORT.md).
  - [Contributing](CONTRIBUTING.md).
  - [Contributor Code of Conduct](CODE_OF_CONDUCT.md).

## Acknowledgments

Thanks to all partners of ForestGEO, for sharing their ideas and code.
Special thanks for inspiring ideas to David Kenfack and Jenny Bryan
([@JennyBryan](https://twitter.com/JennyBryan)).
