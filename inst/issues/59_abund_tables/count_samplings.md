Count saplings
================

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
#> -- Attaching packages -------------------------------------- fgeo 0.0.0.9000 --
#> v fgeo.abundance  0.0.0.9004     v fgeo.demography 0.0.0.9000
#> v fgeo.base       0.0.0.9001     v fgeo.habitat    0.0.0.9006
#> v fgeo.data       0.0.0.9002     v fgeo.map        0.0.0.9204
#> v fgeo.abundance  0.0.0.9004     v fgeo.tool       0.0.0.9003
#> Warning: 'DESCRIPTION' file has an 'Encoding' field and re-encoding is not
#> possible
#> -- Conflicts ---------------------------------------------- fgeo_conflicts() --
#> x fgeo.tool::filter() masks dplyr::filter(), stats::filter()
#> x dplyr::intersect()  masks base::intersect()
#> x dplyr::lag()        masks stats::lag()
#> x dplyr::setdiff()    masks base::setdiff()
#> x dplyr::setequal()   masks base::setequal()
#> x dplyr::union()      masks base::union()
```

> For example, if I want to see how many saplings there are per species,
> the dbh range would be \>=10mm and \<100mm. A tree with multiple stems
> 120 and 20 will be counted, is this correct?

Data: Creating a dataset with one sapling (dbh \>= 10 mm & dbh \< 100
mm) and one tree (dbh \>= 100 mm). The tree has two stems, of 20 mm and
120 mm; the sapling also has two stems, of 22 mm and 99 mm.

``` r
census <- data.frame(
    stringsAsFactors = FALSE,
    dbh = c(20, 120, 22, 99, NA),
    sp = c("sp1", "sp1", "sp2", "sp2", "sp2"),
    treeID = c("1", "1", "2", "2", "2"),
    stemID = c("1.1", "1.2", "2.1", "2.2", "2.3")
)
census
#>   dbh  sp treeID stemID
#> 1  20 sp1      1    1.1
#> 2 120 sp1      1    1.2
#> 3  22 sp2      2    2.1
#> 4  99 sp2      2    2.2
#> 5  NA sp2      2    2.3
```

If we count distinct `treeID`s directly we get a wrong output. The
problem is that the larger stem of the tree is dropped and the smaller
one inluded – making the tree appear like a sappling. We expect a total
of only one saplig but we get two.

``` r
# This preserves missing `dbh` (unless `na.rm = FALSE`)
saplings_bad <- census %>% 
  pick_dbh_min(10) %>% 
  pick_dbh_under(100)
saplings_bad
#>   dbh  sp treeID stemID
#> 1  20 sp1      1    1.1
#> 3  22 sp2      2    2.1
#> 4  99 sp2      2    2.2
#> 5  NA sp2      2    2.3

count_distinct_treeid(saplings_bad)
#>   n
#> 1 2
```

The problem is clearer if we group by species. Note that sp1 should show
zero saplings instead of 1.

``` r
# Count unique instances of treeID by species
saplings_bad %>% 
  group_by(sp) %>% 
  count_distinct_treeid()
#> # A tibble: 2 x 2
#>   sp        n
#>   <chr> <int>
#> 1 sp1       1
#> 2 sp2       1
```

The solution is to collapse the table to a single row per tree –
choosing the one of the stem with the greaterst `dbh`. For that we write
a helper function.

``` r
by_treeid_pick_dbh_max <- function(.data) {
  .data %>% 
    group_by(.data$treeID) %>% 
    arrange(desc(.data$dbh)) %>% 
    filter(row_number() == 1) %>% 
    ungroup()
}
```

Now the output is as expected.

``` r
saplings_good <- census %>% 
  by_treeid_pick_dbh_max() %>% 
  filter(dbh >= 10) %>% 
  filter(dbh < 100)
saplings_good
#> # A tibble: 1 x 4
#>     dbh sp    treeID stemID
#>   <dbl> <chr> <chr>  <chr> 
#> 1    99 sp2   2      2.2

count_distinct_treeid(saplings_good)
#> # A tibble: 1 x 1
#>       n
#>   <int>
#> 1     1
```

-----

> Then I want to count trees per species, i.e. \>=100mm, this tree would
> also be counted by your current function, is this correct?

> If so, this will be wrong, since it will be counted twice. A tree with
> stems 120 and 20 is a tree, not a sapling, and should only be counted
> as a tree.

Again, we only get the correct result if we first collapse the dataset
by choosing the largest stem of each tree.

``` r
# This preserves missing `dbh` (unless `na.rm = FALSE`)
trees_bad <- census %>% 
  pick_dbh_min(100) %>% 
  group_by(sp) %>% 
  count_distinct_treeid()
trees_bad
#> # A tibble: 2 x 2
#>   sp        n
#>   <chr> <int>
#> 1 sp1       1
#> 2 sp2       1

trees_good <- census %>% 
  by_treeid_pick_dbh_max() %>% 
  pick_dbh_min(100) %>% 
  group_by(sp)
trees_good
#> # A tibble: 1 x 4
#> # Groups:   sp [1]
#>     dbh sp    treeID stemID
#>   <dbl> <chr> <chr>  <chr> 
#> 1   120 sp1   1      1.2
count_distinct_treeid(trees_good)
#> # A tibble: 1 x 2
#>   sp        n
#>   <chr> <int>
#> 1 sp1       1
```

Now I can wrap this solution into a user-friendly funtions.
