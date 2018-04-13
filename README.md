
<!-- README.md is generated from README.Rmd. Please edit that file -->

# <img src="https://i.imgur.com/m8FNhQR.png" align="right" height=88 /> fgeo.abundance: Calculate abundance and basal area

[![Travis build
status](https://travis-ci.org/forestgeo/fgeo.abundance.svg?branch=master)](https://travis-ci.org/forestgeo/fgeo.abundance)
[![Coverage
status](https://codecov.io/gh/forestgeo/fgeo.abundance/branch/master/graph/badge.svg)](https://codecov.io/github/forestgeo/fgeo.abundance?branch=master)
[![CRAN
status](http://www.r-pkg.org/badges/version/fgeo.abundance)](https://cran.r-project.org/package=fgeo.abundance)

## Installation

Install **fgeo.abundance** from ForestGEO’s private GitHub repo.

``` r
# install.packages("remotes")
remotes::install_github(repo = "forestgeo/fgeo.abundance")
```

### Example

Abundance and basal area.

``` r
library(fgeo.abundance)
library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union

stem <- tibble(
  quadrat = paste0(0, 1:3),
  sp = letters[1:3],
  status = c("A", "A", "D"),
  dbh = 1:3
)

abundance(stem)
#>   quadrat sp n
#> 1      01  a 1
#> 2      02  b 1

basal_area(stem)
#> Units of returned values are the square of the input units.
#>   quadrat sp basal_area
#> 1      01  a  0.7853982
#> 2      02  b  3.1415927
```
