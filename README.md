
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
# install.packages("devtools")
devtools::install_github(repo = "forestgeo/fgeo.abundance")
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
library(fgeo.tool)
#> 
#> Attaching package: 'fgeo.tool'
#> The following object is masked from 'package:stats':
#> 
#>     filter
library(fgeo.abundance)
```

Abundance

``` r
# Your data may have multiple stems per treeid and even multiple measures per
# stemid (if trees have buttressess).
vft <- tribble(
  ~CensusID, ~TreeID, ~StemID, ~DBH, ~HOM,
          1,     "1",   "1.1",   88,  130,
          1,     "1",   "1.1",   10,  160,
          1,     "2",   "2.1",   20,  130,
          1,     "2",   "2.2",   30,  130,
)

# But you should count only the main stem of each tree
(main_stem <- pick_main_stem(vft))
#> # A tibble: 2 x 5
#>   CensusID TreeID StemID   DBH   HOM
#>      <dbl> <chr>  <chr>  <dbl> <dbl>
#> 1        1 1      1.1       10   160
#> 2        1 2      2.2       30   130
abundance(main_stem)
#> # A tibble: 1 x 1
#>       n
#>   <int>
#> 1     2

# You may have data from multiple censuses
vft2 <- tibble::tribble(
  ~CensusID, ~TreeID, ~StemID, ~DBH, ~HOM,
          1,     "1",   "1.1",   20,  130,
          1,     "1",   "1.2",   10,  160,  # Main stem
          2,     "1",   "1.1",   12,  130,
          2,     "1",   "1.2",   22,  130   # Main stem
)

# You can compute by groups
by_census <- group_by(vft2, CensusID)
(main_stems_by_census <- pick_main_stem(by_census))
#> # A tibble: 2 x 5
#> # Groups:   CensusID [2]
#>   CensusID TreeID StemID   DBH   HOM
#>      <dbl> <chr>  <chr>  <dbl> <dbl>
#> 1        1 1      1.2       10   160
#> 2        2 1      1.2       22   130
abundance(main_stems_by_census)
#> # A tibble: 2 x 2
#> # Groups:   CensusID [2]
#>   CensusID     n
#>      <dbl> <int>
#> 1        1     1
#> 2        2     1

# You may need to first subset data and then count
over20 <- filter(main_stems_by_census, DBH > 20)
abundance(over20)
#> # A tibble: 1 x 2
#> # Groups:   CensusID [1]
#>   CensusID     n
#>      <dbl> <int>
#> 1        2     1
```

Basal area

``` r
vft <- tribble(
  ~CensusID, ~TreeID, ~StemID, ~DBH, ~HOM,
          1,     "1",   "1.1",   88,  130,
          1,     "1",   "1.1",   10,  160,
          1,     "2",   "2.1",   20,  130,
          1,     "2",   "2.2",   30,  130,
)

# You may need to pick the main stemid of each stem (if trees have buttressess)
(main_stemids <- pick_main_stemid(vft))
#> # A tibble: 3 x 5
#>   CensusID TreeID StemID   DBH   HOM
#>      <dbl> <chr>  <chr>  <dbl> <dbl>
#> 1        1 1      1.1       10   160
#> 2        1 2      2.1       20   130
#> 3        1 2      2.2       30   130
basal_area(main_stemids)
#> # A tibble: 1 x 1
#>   basal_area
#>        <dbl>
#> 1      1100.

# You can compute by groups
basal_area(by_census)
#> # A tibble: 2 x 2
#> # Groups:   CensusID [2]
#>   CensusID basal_area
#>      <dbl>      <dbl>
#> 1        1       393.
#> 2        2       493.

ten_to_twenty <- filter(by_census, DBH >= 10, DBH <= 20)
basal_area(ten_to_twenty)
#> # A tibble: 2 x 2
#> # Groups:   CensusID [2]
#>   CensusID basal_area
#>      <dbl>      <dbl>
#> 1        1       393.
#> 2        2       113.
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
