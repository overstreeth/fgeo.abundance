Counting trees and saplings
================

This document reports the progress on functions to count trees (stems of
dbh 100 mm and above) and saplings (stems of dbh between 10 mm inclusive
and 100 mm exclusive). It follows up a discussion with Suzanne Lao, in
which she identified a bug in an earlier version of the code. Now, as
far as I know, the issue is fixed. The solution was to develop a feature
that Suzanne had requested: To pick the stem of largest dbh of each tree
before doing any other filtering on dbh. Here I show the issue and the
fix.

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
library(fgeo)
#> -- Attaching packages ---------------------------------------------- fgeo 0.0.0.9000 --
#> v fgeo.abundance  0.0.0.9004     v fgeo.demography 0.0.0.9000
#> v fgeo.base       0.0.0.9001     v fgeo.habitat    0.0.0.9006
#> v fgeo.data       0.0.0.9002     v fgeo.map        0.0.0.9204
#> v fgeo.abundance  0.0.0.9004     v fgeo.tool       0.0.0.9003
#> 
```

Data: Creating a dataset with one sapling (dbh \>= 10 mm & dbh \< 100
mm) and one tree (dbh \>= 100 mm). The tree has two stems, of 20 mm and
120 mm; the sapling also has two stems, of 22 mm and 99 mm.

``` r
census <- tibble(
    stringsAsFactors = FALSE,
    sp = c("sp1", "sp1", "sp2", "sp2", "sp2"),
    treeID = c("1", "1", "2", "2", "2"),
    stemID = c("1.1", "1.2", "2.1", "2.2", "2.3"),
    dbh = c(20, 120, 22, 99, NA)
)
census
#> # A tibble: 5 x 5
#>   stringsAsFactors sp    treeID stemID   dbh
#>   <lgl>            <chr> <chr>  <chr>  <dbl>
#> 1 FALSE            sp1   1      1.1       20
#> 2 FALSE            sp1   1      1.2      120
#> 3 FALSE            sp2   2      2.1       22
#> 4 FALSE            sp2   2      2.2       99
#> 5 FALSE            sp2   2      2.3       NA
```

If we count distinct `treeID`s directly we get a wrong output. The
problem is that the larger stem of the tree is dropped and the smaller
one included – making the tree appear like a sapling. We expect a total
of only one sapling but we get two.

``` r
# This preserves missing `dbh` (unless `na.rm = FALSE`)
saplings_bad <- census %>% 
  pick_dbh_min(10) %>% 
  pick_dbh_under(100)
saplings_bad
#> # A tibble: 4 x 5
#>   stringsAsFactors sp    treeID stemID   dbh
#>   <lgl>            <chr> <chr>  <chr>  <dbl>
#> 1 FALSE            sp1   1      1.1       20
#> 2 FALSE            sp2   2      2.1       22
#> 3 FALSE            sp2   2      2.2       99
#> 4 FALSE            sp2   2      2.3       NA

count_distinct(saplings_bad, treeID)
#> # A tibble: 1 x 1
#>       n
#>   <int>
#> 1     2
```

The problem is clearer if we group by species. Note that sp1 should show
zero saplings instead of 1.

``` r
# Count unique instances of treeID by species
saplings_bad %>% 
  group_by(sp) %>% 
  count_distinct(treeID)
#> # A tibble: 2 x 2
#>   sp        n
#>   <chr> <int>
#> 1 sp1       1
#> 2 sp2       1
```

The solution is to collapse the table to a single row per tree –
choosing the one of the stem with the largest `dbh`.

``` r
largest <- census %>% pick_dbh_largest()
largest
#> # A tibble: 2 x 5
#>   stringsAsFactors sp    treeID stemID   dbh
#>   <lgl>            <chr> <chr>  <chr>  <dbl>
#> 1 FALSE            sp1   1      1.2      120
#> 2 FALSE            sp2   2      2.2       99

saplings_good <- largest %>% 
  filter(dbh >= 10) %>% 
  filter(dbh < 100)
saplings_good
#> # A tibble: 1 x 5
#>   stringsAsFactors sp    treeID stemID   dbh
#>   <lgl>            <chr> <chr>  <chr>  <dbl>
#> 1 FALSE            sp2   2      2.2       99
```

Now the output is as expected.

``` r
count_distinct_treeid(saplings_good)
#> # A tibble: 1 x 1
#>       n
#>   <int>
#> 1     1

saplings_good %>% 
  group_by(sp) %>% 
  count_distinct_treeid()
#> # A tibble: 1 x 2
#>   sp        n
#>   <chr> <int>
#> 1 sp2       1
```

Above we picked saplings with the new function `pick_dbh_largest()` and
`dplyr::filter()`. That is quite simple but, considering this may be a
very common task, we can make it even simpler with the new specialized
functions `count_woods()`, and its friends `count_saplings()` and
`count_trees()`.

The new workflow to count saplings is this; first across the entire
dataset and then by species:

``` r
census %>% count_saplings()
#> # A tibble: 1 x 1
#>       n
#>   <int>
#> 1     1

census %>% 
  group_by(sp) %>% 
  count_saplings()
#> # A tibble: 2 x 2
#> # Groups:   sp [2]
#>   sp        n
#>   <chr> <int>
#> 1 sp1       0
#> 2 sp2       1
```

And this is the workflow for counting trees:

``` r
census %>% count_trees()
#> # A tibble: 1 x 1
#>       n
#>   <int>
#> 1     1

census %>% 
  group_by(sp) %>% 
  count_trees()
#> # A tibble: 2 x 2
#> # Groups:   sp [2]
#>   sp        n
#>   <chr> <int>
#> 1 sp1       1
#> 2 sp2       0
```

Finally, this is the most general approach:

  - Saplings

<!-- end list -->

``` r
# Saplings
census %>% count_woods(dbh >= 10, dbh < 100)
#> # A tibble: 1 x 1
#>       n
#>   <int>
#> 1     1

census %>%
  group_by(sp) %>%
  count_woods(dbh >= 100)
#> # A tibble: 2 x 2
#> # Groups:   sp [2]
#>   sp        n
#>   <chr> <int>
#> 1 sp1       1
#> 2 sp2       0
```

  - Trees

<!-- end list -->

``` r
census %>% count_woods(dbh >= 100)
#> # A tibble: 1 x 1
#>       n
#>   <int>
#> 1     1

census %>% 
  group_by(sp) %>%
  count_woods(dbh >= 100)
#> # A tibble: 2 x 2
#> # Groups:   sp [2]
#>   sp        n
#>   <chr> <int>
#> 1 sp1       1
#> 2 sp2       0
```
