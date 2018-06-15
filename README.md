
<!-- README.md is generated from README.Rmd. Please edit that file -->

# <img src="https://i.imgur.com/m8FNhQR.png" align="right" height=88 /> Abundance, basal area and diversity

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
library(fgeo.base)
library(fgeo.abundance)

stem <- fgeo.data::luquillo_stem_random_tiny
```

``` r
pick <- pick_dbh_min(stem, 10)
pick <- drop_status(pick, "G")
# Same
pick <- stem %>% 
  pick_dbh_min(10) %>% 
  drop_status("G")

pick
#> # A tibble: 67 x 19
#>    treeID stemID tag   StemTag sp     quadrat    gx    gy MeasureID
#>     <int>  <int> <chr> <chr>   <chr>  <chr>   <dbl> <dbl>     <int>
#>  1  10775  13496 1128  1128    PREMON 402      79.3  39.8     13496
#>  2  10863  13603 1129  1129    PREMON 402      79.0  36.5     13603
#>  3  23710  29640 14434 14434   PREMON 517      82.7 330.      29640
#>  4  25331  31937 16164 16164   SLOBER 402      70.8  21.0     31937
#>  5  63997  77760 75062 75062   CASARB 517      86.0 340.      77760
#>  6  63997  77761 75062 75063   CASARB 517      86.0 340.      77761
#>  7  63997  77762 75062 75064   CASARB 517      86.0 340.      77762
#>  8  63997  77763 75062 75065   CASARB 517      86.0 340.      77763
#>  9  63997  77764 75062 75066   CASARB 517      86.0 340.      77764
#> 10  64070  77850 75147 75147   CASARB 517      99.8 325.      77850
#> # ... with 57 more rows, and 10 more variables: CensusID <int>, dbh <dbl>,
#> #   pom <chr>, hom <dbl>, ExactDate <dbl>, DFstatus <chr>, codes <chr>,
#> #   countPOM <dbl>, status <chr>, date <dbl>
```

### Basal area

``` r
# Add basal area to a dataframe
ba <- basal_area(pick$dbh)
pick$ba <- ba
# Same in one step
pick <- mutate(pick, ba = basal_area(dbh))
# Reordering columns to show `ba` first
select(pick, ba, everything())
#> # A tibble: 67 x 20
#>        ba treeID stemID tag   StemTag sp     quadrat    gx    gy MeasureID
#>     <dbl>  <int>  <int> <chr> <chr>   <chr>  <chr>   <dbl> <dbl>     <int>
#>  1 18869.  10775  13496 1128  1128    PREMON 402      79.3  39.8     13496
#>  2 22167.  10863  13603 1129  1129    PREMON 402      79.0  36.5     13603
#>  3 10936.  23710  29640 14434 14434   PREMON 517      82.7 330.      29640
#>  4  1995.  25331  31937 16164 16164   SLOBER 402      70.8  21.0     31937
#>  5  2043.  63997  77760 75062 75062   CASARB 517      86.0 340.      77760
#>  6   254.  63997  77761 75062 75063   CASARB 517      86.0 340.      77761
#>  7   314.  63997  77762 75062 75064   CASARB 517      86.0 340.      77762
#>  8   398.  63997  77763 75062 75065   CASARB 517      86.0 340.      77763
#>  9   594.  63997  77764 75062 75066   CASARB 517      86.0 340.      77764
#> 10   113.  64070  77850 75147 75147   CASARB 517      99.8 325.      77850
#> # ... with 57 more rows, and 10 more variables: CensusID <int>, dbh <dbl>,
#> #   pom <chr>, hom <dbl>, ExactDate <dbl>, DFstatus <chr>, codes <chr>,
#> #   countPOM <dbl>, status <chr>, date <dbl>

# Summarize basal area
basal_area(pick)
#> # A tibble: 1 x 1
#>   basal_area
#>        <dbl>
#> 1    586336.

# Now by groups
basal_area(group_by(pick, quadrat))
#> # A tibble: 2 x 2
#>   quadrat basal_area
#>   <chr>        <dbl>
#> 1 402        340213.
#> 2 517        246123.
```

### Abundance

``` r
# Summarize abundance (rows count) by quadrat by species
abundance <- count(pick, quadrat, sp)
abundance
#> # A tibble: 4 x 3
#>   quadrat sp         n
#>   <chr>   <chr>  <int>
#> 1 402     PREMON    16
#> 2 402     SLOBER    16
#> 3 517     CASARB    22
#> 4 517     PREMON    13

# add_count() is useful for groupwise filtering. E.g.: to pick all data from
# species which abundance per quadrat is under some treshold
with_abundance <- add_count(pick, quadrat, sp)
under_treshold <- filter(with_abundance, n < 20)
select(under_treshold, quadrat, sp, n, everything())
#> # A tibble: 45 x 21
#>    quadrat sp         n treeID stemID tag   StemTag    gx    gy MeasureID
#>    <chr>   <chr>  <int>  <int>  <int> <chr> <chr>   <dbl> <dbl>     <int>
#>  1 402     PREMON    16  10775  13496 1128  1128     79.3  39.8     13496
#>  2 402     PREMON    16  10863  13603 1129  1129     79.0  36.5     13603
#>  3 517     PREMON    13  23710  29640 14434 14434    82.7 330.      29640
#>  4 402     SLOBER    16  25331  31937 16164 16164    70.8  21.0     31937
#>  5 402     SLOBER    16  69235  84589 8166  8166     65.8  20.8     84589
#>  6 402     PREMON    16  10775  13496 1128  1128     79.3  39.8    116871
#>  7 402     PREMON    16  10863  13603 1129  1129     79.0  36.5    116978
#>  8 517     PREMON    13  23710  29640 14434 14434    82.7 330.     133014
#>  9 402     SLOBER    16  25331  31937 16164 16164    70.8  21.0    135311
#> 10 402     SLOBER    16  69235  84589 8166  8166     65.8  20.8    187959
#> # ... with 35 more rows, and 11 more variables: CensusID <int>, dbh <dbl>,
#> #   pom <chr>, hom <dbl>, ExactDate <dbl>, DFstatus <chr>, codes <chr>,
#> #   countPOM <dbl>, status <chr>, date <dbl>, ba <dbl>
```

### Metrics of diversity

``` r
vgn_diversity(abundance, n)
#> # A tibble: 2 x 2
#>   index      value
#>   <chr>      <dbl>
#> 1 specnumber  4   
#> 2 shannon     1.37

# Same result, different format
vegan::specnumber(abundance$n)
#> [1] 4
vegan::diversity(abundance$n, "shannon")
#> [1] 1.367825
```

### More ways to count things

`janitor::tabyl()` and `base::table()` are useful tools for counting
things. Their output is sometimes best suited for some analyses, for
example with **vegan**.

``` r
if (!requireNamespace("janitor", quietly = TRUE)) {
  stop(
    "This section uses janitor, install it with:\n",
    "install.packages('janitor')"
  )
}
library(janitor)

# Compare
count(pick, quadrat, sp)
#> # A tibble: 4 x 3
#>   quadrat sp         n
#>   <chr>   <chr>  <int>
#> 1 402     PREMON    16
#> 2 402     SLOBER    16
#> 3 517     CASARB    22
#> 4 517     PREMON    13

tabyl(pick, quadrat, sp)
#>  quadrat CASARB PREMON SLOBER
#>      402      0     16     16
#>      517     22     13      0

# Useful for vegan
freq <- table(pick$quadrat, pick$sp)
freq
#>      
#>       CASARB PREMON SLOBER
#>   402      0     16     16
#>   517     22     13      0

vegan::specaccum(freq)
#> Warning in cor(x > 0): the standard deviation is zero
#> Species Accumulation Curve
#> Accumulation method: exact
#> Call: vegan::specaccum(comm = freq) 
#> 
#>             
#> Sites    1 2
#> Richness 2 3
#> sd       0 0

vegan::diversity(freq, "shannon")
#>       402       517 
#> 0.6931472 0.6597116
```

`janitor::tabyl()` works well with data in 3 dimensions; it outputs a
list of dataframes, which are often easier to work with than the output
of `base::table()`.

``` r
tabyl(pick, CensusID, quadrat, sp)
#> $CASARB
#>  CensusID 402 517
#>         1   0   6
#>         2   0   3
#>         3   0   6
#>         4   0   4
#>         5   0   2
#>         6   0   1
#> 
#> $PREMON
#>  CensusID 402 517
#>         1   2   1
#>         2   2   1
#>         3   3   2
#>         4   3   3
#>         5   3   3
#>         6   3   3
#> 
#> $SLOBER
#>  CensusID 402 517
#>         1   2   0
#>         2   2   0
#>         3   5   0
#>         4   4   0
#>         5   3   0
#>         6   0   0

tabyl(pick, CensusID, quadrat, sp, show_missing_levels = FALSE)
#> $CASARB
#>  CensusID 517
#>         1   6
#>         2   3
#>         3   6
#>         4   4
#>         5   2
#>         6   1
#> 
#> $PREMON
#>  CensusID 402 517
#>         1   2   1
#>         2   2   1
#>         3   3   2
#>         4   3   3
#>         5   3   3
#>         6   3   3
#> 
#> $SLOBER
#>  CensusID 402
#>         1   2
#>         2   2
#>         3   5
#>         4   4
#>         5   3
```

And makes it easy to format tables by using a declarative style, best
suited for composing multiple operations with the pipe (`%>%`) and
produces neat output with `knitr::kable()`.

``` r
tab <- tabyl(pick, quadrat, sp)
tab %>% 
  adorn_totals("row") %>% 
  adorn_percentages() %>% 
  adorn_pct_formatting() %>% 
  adorn_title("top") %>% 
  knitr::kable()
```

|         | sp     |        |        |
| ------- | :----- | ------ | ------ |
| quadrat | CASARB | PREMON | SLOBER |
| 402     | 0.0%   | 50.0%  | 50.0%  |
| 517     | 62.9%  | 37.1%  | 0.0%   |
| Total   | 32.8%  | 43.3%  | 23.9%  |

## Information

  - [Getting help](SUPPORT.md).
  - [Contributing](CONTRIBUTING.md).
  - [Contributor Code of Conduct](CODE_OF_CONDUCT.md).

## Acknowledgments

Thanks to all partners of ForestGEO, for sharing their ideas and code.
Special thanks for inspiring ideas to David Kenfack and Jenny Bryan
([@JennyBryan](https://twitter.com/JennyBryan)).
