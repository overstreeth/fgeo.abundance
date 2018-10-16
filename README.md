
<!-- README.md is generated from README.Rmd. Please edit that file -->

# <img src="https://i.imgur.com/vTLlhbp.png" align="right" height=88 /> Abundance and basal area

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

### Abundance

Your data may have multiple stems per treeid and even multiple measures
per stemid (if trees have
buttresses).

``` r
# Trees with buttresses may have multiple measurements of a single stem. 
# Main stems have highest `HOM`, then largest `DBH`.
vft <- tribble(
  ~CensusID, ~TreeID, ~StemID, ~DBH, ~HOM,
          1,     "1",   "1.1",   88,  130,
          1,     "1",   "1.1",   10,  160,  # Main stem
          1,     "2",   "2.1",   20,  130,
          1,     "2",   "2.2",   30,  130,  # Main stem
)
```

Fundamentally, `abundance()` counts rows. All of these results are the
same:

``` r
nrow(vft)
#> [1] 4
dplyr::count(vft)
#> # A tibble: 1 x 1
#>       n
#>   <int>
#> 1     4
dplyr::summarize(vft, n = n())
#> # A tibble: 1 x 1
#>       n
#>   <int>
#> 1     4
abundance(vft)
#> Warning: `treeid`: Duplicated values were detected. Do you need to pick
#> main stems?
#> # A tibble: 1 x 1
#>       n
#>   <int>
#> 1     4
```

But that result is likely not what you expect. Instead, you likely
expect this:

``` r
summarize(vft, n = n_distinct(TreeID))
#> # A tibble: 1 x 1
#>       n
#>   <int>
#> 1     2
```

A shown above, you can get a correct result by combining `summarize()`
and `n_distinct()` (from the **dplyr** package). But `abundance()`
includes some useful additional features (see `?abundance()`). This code
more clearly conveys your intention, i.e.g to calculate tree abundance
by counting the number of main stems:

``` r
(main_stems <- pick_main_stem(vft))
#> # A tibble: 2 x 5
#>   CensusID TreeID StemID   DBH   HOM
#>      <dbl> <chr>  <chr>  <dbl> <dbl>
#> 1        1 1      1.1       10   160
#> 2        1 2      2.2       30   130
abundance(main_stems)
#> # A tibble: 1 x 1
#>       n
#>   <int>
#> 1     2
```

If you have data from multiple censuses, you can compute by census (or
any other group).

``` r
vft2 <- tibble::tribble(
  ~CensusID, ~TreeID, ~StemID, ~DBH, ~HOM,
          1,     "1",   "1.1",   10,  130,
          1,     "1",   "1.2",   20,  130,  # Main stem
          2,     "1",   "1.1",   12,  130,
          2,     "1",   "1.2",   22,  130   # Main stem
)

by_census <- group_by(vft2, CensusID)
(main_stems_by_census <- pick_main_stem(by_census))
#> # A tibble: 2 x 5
#> # Groups:   CensusID [2]
#>   CensusID TreeID StemID   DBH   HOM
#>      <dbl> <chr>  <chr>  <dbl> <dbl>
#> 1        1 1      1.2       20   130
#> 2        2 1      1.2       22   130
abundance(main_stems_by_census)
#> # A tibble: 2 x 2
#> # Groups:   CensusID [2]
#>   CensusID     n
#>      <dbl> <int>
#> 1        1     1
#> 2        2     1
```

Often you will need to first subset data (e.g. by `status` or `DBH`) and
then count.

``` r
over20 <- filter(main_stems_by_census, DBH > 20)
abundance(over20)
#> # A tibble: 1 x 2
#> # Groups:   CensusID [1]
#>   CensusID     n
#>      <dbl> <int>
#> 1        2     1
```

### Basal area

If trees have buttresses, you may need to pick the main stemid of each
stem so you don’t count the same stem more than once.

``` r
vft3 <- tribble(
  ~CensusID, ~TreeID, ~StemID, ~DBH, ~HOM,
          1,     "1",   "1.1",   88,  130,
          1,     "1",   "1.1",   10,  160,  # Main stem
          1,     "2",   "2.1",   20,  130,
          1,     "2",   "2.2",   30,  130,  # Main stem

          2,     "1",   "1.1",   98,  130,
          2,     "1",   "1.1",   20,  160,  # Main stem
          2,     "2",   "2.1",   30,  130,
          2,     "2",   "2.2",   40,  130,  # Main stem
)

(main_stemids <- pick_main_stemid(vft3))
#> # A tibble: 6 x 5
#>   CensusID TreeID StemID   DBH   HOM
#>      <dbl> <chr>  <chr>  <dbl> <dbl>
#> 1        1 1      1.1       10   160
#> 2        1 2      2.1       20   130
#> 3        1 2      2.2       30   130
#> 4        2 1      1.1       20   160
#> 5        2 2      2.1       30   130
#> 6        2 2      2.2       40   130
main_stemids
#> # A tibble: 6 x 5
#>   CensusID TreeID StemID   DBH   HOM
#>      <dbl> <chr>  <chr>  <dbl> <dbl>
#> 1        1 1      1.1       10   160
#> 2        1 2      2.1       20   130
#> 3        1 2      2.2       30   130
#> 4        2 1      1.1       20   160
#> 5        2 2      2.1       30   130
#> 6        2 2      2.2       40   130

basal_area(main_stemids)
#> Warning: `stemid`: Duplicated values were detected. Do you need to pick
#> largest `hom` values?
#> Warning: `censusid`: Multiple values were detected. Do you need to group by
#> censusid?
#> # A tibble: 1 x 1
#>   basal_area
#>        <dbl>
#> 1      3377.
```

`basal_area()` also allows you to compute by groups.

``` r
by_census <- group_by(main_stemids, CensusID)
basal_area(by_census)
#> # A tibble: 2 x 2
#> # Groups:   CensusID [2]
#>   CensusID basal_area
#>      <dbl>      <dbl>
#> 1        1      1100.
#> 2        2      2278.
```

But if you want to compute on a subset of data, you need to first pick
the data.

``` r
ten_to_twenty <- filter(by_census, DBH >= 10, DBH <= 20)
ba <- basal_area(ten_to_twenty)
ba
#> # A tibble: 2 x 2
#> # Groups:   CensusID [2]
#>   CensusID basal_area
#>      <dbl>      <dbl>
#> 1        1       393.
#> 2        2       314.
```

And if you need to convert units, you can do so after the fact with
`fgeo.tool::convert_unit_at()`.

``` r
convert_unit_at(ba, .at = "basal_area", from = "mm2", to = "hectare")
#> # A tibble: 2 x 2
#> # Groups:   CensusID [2]
#>   CensusID   basal_area
#>      <dbl>        <dbl>
#> 1        1 0.0000000393
#> 2        2 0.0000000314
```

### Summaries aggregated by year

Example data.

``` r
vft <- example_byyr
vft
#> # A tibble: 8 x 13
#>   PlotName CensusID TreeID StemID Status   DBH Genus SpeciesName ExactDate 
#>   <chr>       <int>  <int>  <dbl> <chr>  <int> <chr> <chr>       <date>    
#> 1 luq             1      1    1.1 alive     10 Gn    spp         2001-01-01
#> 2 luq             1      1    1.2 dead      NA Gn    spp         2001-01-01
#> 3 luq             1      2    2.1 alive     20 Gn    spp         2001-01-01
#> 4 luq             1      2    2.2 alive     30 Gn    spp         2001-01-01
#> 5 luq             2      1    1.1 alive     20 Gn    spp         2002-01-01
#> 6 luq             2      1    1.2 gone      NA Gn    spp         2002-01-01
#> 7 luq             2      2    2.1 dead      NA Gn    spp         2002-01-01
#> 8 luq             2      2    2.2 dead      NA Gn    spp         2002-01-01
#> # ... with 4 more variables: PlotCensusNumber <int>, Family <chr>,
#> #   Tag <int>, HOM <int>
```

Abundance by year.

``` r
abundance_byyr(vft, DBH >= 10, DBH < 20)
#> # A tibble: 1 x 3
#>   species family yr_2001
#>   <chr>   <chr>    <dbl>
#> 1 Gn spp  f            1

abundance_byyr(vft, DBH >= 10)
#> # A tibble: 1 x 4
#>   species family yr_2001 yr_2002
#>   <chr>   <chr>    <dbl>   <dbl>
#> 1 Gn spp  f            2       1
```

Basal area by year.

``` r
basal <- basal_area_byyr(vft, DBH >= 10)
basal
#> # A tibble: 1 x 4
#>   species family yr_2001 yr_2002
#>   <chr>   <chr>    <dbl>   <dbl>
#> 1 Gn spp  f        1100.    314.

# Convert units and standardize by plot size in hectares
years <- c("yr_2001", "yr_2002")
in_he <- convert_unit_at(basal, .at = years, from = "mm2", to = "hectare")
standardize_at(in_he, .at = years, denominator = 50)
#> # A tibble: 1 x 4
#>   species family       yr_2001  yr_2002
#>   <chr>   <chr>          <dbl>    <dbl>
#> 1 Gn spp  f      0.00000000220 6.28e-10
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
