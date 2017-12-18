
<!-- README.md is generated from README.Rmd. Please edit that file -->
fgeo.abundance: Abundance and basal area <img src="https://i.imgur.com/39pvr4n.png" align="right" height=44 />
==============================================================================================================

[![Travis build status](https://travis-ci.org/forestgeo/fgeo.abundance.svg?branch=master)](https://travis-ci.org/forestgeo/fgeo.abundance) [![Coverage status](https://codecov.io/gh/forestgeo/fgeo.abundance/branch/master/graph/badge.svg)](https://codecov.io/github/forestgeo/fgeo.abundance?branch=master) [![CRAN status](http://www.r-pkg.org/badges/version/fgeo.abundance)](https://cran.r-project.org/package=fgeo.abundance)

Installation
------------

Install **fgeo.abundance** from ForestGEO's private GitHub repo.

``` r
# To install from a private repo, see auth_token at https://goo.gl/re1LFe
# install.packages("remotes")
remotes::install_github(repo = "forestgeo/fgeo.abundance")
```

### Example

Abundance and basal area.

``` r
library(fgeo.abundance)

stem <- bciex::bci12s7mini

head(abundance(stem))
#>   quadrat     sp n
#> 1    0000 soroaf 1
#> 2    0002 hybapr 1
#> 3    0006 hybapr 2
#> 4    0009 faraoc 1
#> 5    0009 hybapr 1
#> 6    0020 tet2pa 1

head(basal_area(stem))
#> Units of returned values are the square of the input units.
#>   quadrat     sp basal_area
#> 1    0000 soroaf 2642.07942
#> 2    0002 hybapr   95.03318
#> 3    0006 hybapr  475.16589
#> 4    0009 faraoc 1809.55737
#> 5    0009 hybapr    0.00000
#> 6    0020 tet2pa 3117.24531
```
