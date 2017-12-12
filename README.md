
<!-- README.md is generated from README.Rmd. Please edit that file -->
forestr: analyse abundance and demography <img src="https://i.imgur.com/39pvr4n.png" align="right" height=44 />
===============================================================================================================

[![Travis build status](https://travis-ci.org/forestgeo/forestr.svg?branch=master)](https://travis-ci.org/forestgeo/forestr) [![Coverage status](https://codecov.io/gh/forestgeo/forestr/branch/master/graph/badge.svg)](https://codecov.io/github/forestgeo/forestr?branch=master) [![CRAN status](http://www.r-pkg.org/badges/version/forestr)](https://cran.r-project.org/package=forestr)

Installation
------------

Install forestr from ForestGEO's private GitHub repo.

``` r
# To install from a private repo, see auth_token at https://goo.gl/re1LFe
# install.packages("remotes")
remotes::install_github(repo = "forestgeo/forestr")
```

### Example

Abundance and basal area.

``` r
library(forestr)

stem <- forestr::bci12s7mini

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

Demography: recruitment and mortality.

``` r
# Some data to play with
census1 <- forestr::bci12t6mini
census2 <- forestr::bci12t7mini

recruitment(census1, census2)
#> $N2
#> [1] 554
#> 
#> $R
#> [1] 71
#> 
#> $rate
#> [1] 0.02759928
#> 
#> $lower
#> [1] 0.02185356
#> 
#> $upper
#> [1] 0.03475338
#> 
#> $time
#> [1] 4.969261
#> 
#> $date1
#> [1] 16578.29
#> 
#> $date2
#> [1] 18393.99

# Easier to view and filter when you split by some variable with many groups
# (note warning when some groups have dbh values full of NA)
recruitment_df(census1, census2, split1 = census1$sp)
#> Warning in check_if_all_dbh_is_na(census1, census2, split = split1): At least one split-group contains all `dbh` values equal to NA.
#>   * Consider removing those groups before running this function.
#> # A tibble: 1,152 x 3
#>     split metric        value
#>     <chr>  <chr>        <dbl>
#>  1 acaldi     N2 5.000000e+00
#>  2 acaldi      R 2.000000e+00
#>  3 acaldi   rate 1.032320e-01
#>  4 acaldi  lower 2.540176e-02
#>  5 acaldi  upper 3.034520e-01
#>  6 acaldi   time 4.948324e+00
#>  7 acaldi  date1 1.663900e+04
#>  8 acaldi  date2 1.846360e+04
#>  9 aegipa     N2 0.000000e+00
#> 10 aegipa      R 0.000000e+00
#> # ... with 1,142 more rows

# Same for `mortality()`; lets explore a few species
(all_sp <- mortality_df(census1, census2, split1 = census1$sp))
#> Warning in check_if_all_dbh_is_na(census1, census2, split = split1): At least one split-group contains all `dbh` values equal to NA.
#>   * Consider removing those groups before running this function.
#> # A tibble: 1,296 x 3
#>     split  metric        value
#>     <chr>   <chr>        <dbl>
#>  1 acaldi       N 3.000000e+00
#>  2 acaldi       D 0.000000e+00
#>  3 acaldi    rate 0.000000e+00
#>  4 acaldi   lower 0.000000e+00
#>  5 acaldi   upper 1.867879e-01
#>  6 acaldi    time 4.937258e+00
#>  7 acaldi   date1 1.663900e+04
#>  8 acaldi   date2 1.844233e+04
#>  9 acaldi dbhmean 1.300000e+01
#> 10 aegipa       N 0.000000e+00
#> # ... with 1,286 more rows
few_sp <- sample(unique(all_sp$split), 3)
long_format <- subset(all_sp, split %in% few_sp)
head(long_format, 10)
#> # A tibble: 10 x 3
#>     split  metric        value
#>     <chr>   <chr>        <dbl>
#>  1 beilpe       N 4.000000e+00
#>  2 beilpe       D 0.000000e+00
#>  3 beilpe    rate 0.000000e+00
#>  4 beilpe   lower 0.000000e+00
#>  5 beilpe   upper 1.487977e-01
#>  6 beilpe    time 4.958248e+00
#>  7 beilpe   date1 1.656325e+04
#>  8 beilpe   date2 1.837425e+04
#>  9 beilpe dbhmean 7.900000e+01
#> 10 ocotwh       N 1.000000e+00
```
