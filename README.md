
<!-- README.md is generated from README.Rmd. Please edit that file -->

# <img src="https://i.imgur.com/m8FNhQR.png" align="right" height=88 /> Abundance and basal area

[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![Travis build
status](https://travis-ci.org/forestgeo/fgeo.abundance.svg?branch=master)](https://travis-ci.org/forestgeo/fgeo.abundance)
[![Coverage
status](https://codecov.io/gh/forestgeo/fgeo.abundance/branch/master/graph/badge.svg)](https://codecov.io/github/forestgeo/fgeo.abundance?branch=master)
[![CRAN
status](http://www.r-pkg.org/badges/version/fgeo.abundance)](https://cran.r-project.org/package=fgeo.abundance)

**fgeo.abundance** calculates abundance, basal area and diversity.

## Installation

``` r
# install.packages("remotes")
remotes::install_github(repo = "forestgeo/fgeo.abundance")
```

For details on how to install packages from GitHub, see [this
article](https://goo.gl/dQKEeg).

## Example

Abundance and basal area.

``` r
library(fgeo.abundance)

stem <- data.frame(
  quadrat = paste0(0, 1:3),
  sp = letters[1:3],
  status = c("A", "A", "D"),
  dbh = 1:3,
  stringsAsFactors = FALSE
)
stem
#>   quadrat sp status dbh
#> 1      01  a      A   1
#> 2      02  b      A   2
#> 3      03  c      D   3

basal_area(stem)
#> Units of returned values are the square of the input units.
#>   quadrat sp basal_area
#> 1      01  a  0.7853982
#> 2      02  b  3.1415927

abund <- abundance(stem)
abund
#>   quadrat sp n
#> 1      01  a 1
#> 2      02  b 1

vgn_diversity(abund, n)
#>        index     value
#> 1 specnumber 2.0000000
#> 2    shannon 0.6931472
```

## Information

  - [Getting help](SUPPORT.md).
  - [Contributing](CONTRIBUTING.md).
  - [Contributor Code of Conduct](CODE_OF_CONDUCT.md).

## Acknowledgments

Thanks to all partners of ForestGEO, for sharing their ideas and code.
Special thanks for inspiring ideas to David Kenfack and Jenny Bryan
([@JennyBryan](https://twitter.com/JennyBryan)).
