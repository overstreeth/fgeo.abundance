Count saplings
================

``` r
library(fgeo)
#> -- Attaching packages ------------------------------------------- fgeo 0.0.0.9000 --
#> v fgeo.abundance  0.0.0.9004     v fgeo.demography 0.0.0.9000
#> v fgeo.base       0.0.0.9001     v fgeo.habitat    0.0.0.9006
#> v fgeo.data       0.0.0.9002     v fgeo.map        0.0.0.9204
#> v fgeo.abundance  0.0.0.9004     v fgeo.tool       0.0.0.9003
#> -- Conflicts --------------------------------------------------- fgeo_conflicts() --
#> x fgeo.tool::filter() masks stats::filter()
```

Data.

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

> For example, if I want to see how many saplings there are per species,
> the dbh range would be \>=10mm and \<100mm. A tree with multiple stems
> 120 and 20 will be counted, is this correct?

Counting saplings by species.

``` r
# This preserves missing `dbh` (unless `na.rm = FALSE`)
saplings <- census %>% 
  pick_dbh_min(10) %>% 
  pick_dbh_under(100)
saplings
#>   dbh  sp treeID stemID
#> 1  20 sp1      1    1.1
#> 3  22 sp2      2    2.1
#> 4  99 sp2      2    2.2
#> 5  NA sp2      2    2.3

# Count unique instances of treeID by species
saplings %>% 
  group_by(sp) %>% 
  abundance_tree()
#> # A tibble: 2 x 2
#>   sp        n
#>   <chr> <int>
#> 1 sp1       1
#> 2 sp2       1
```

Bonus:

  - Other ways of filtering desired dbh.

<!-- end list -->

``` r
saplings <- filter(census, dbh >= 10, dbh < 100)
# Same
saplings <- census %>% 
  filter(dbh >= 10, dbh < 100)
# Same
saplings <- census %>% 
  filter(dbh >= 10) %>% 
  filter(dbh < 100)
saplings
#>   dbh  sp treeID stemID
#> 1  20 sp1      1    1.1
#> 2  22 sp2      2    2.1
#> 3  99 sp2      2    2.2
```

  - Other counts.

<!-- end list -->

``` r
# Count unique instances of treeID
abundance_tree(saplings)
#>   n
#> 1 2

# Count unique instances of stemID by species
saplings %>% 
  group_by(sp) %>% 
  abundance_stem()
#> # A tibble: 2 x 2
#>   sp        n
#>   <chr> <int>
#> 1 sp1       1
#> 2 sp2       2
```

> Then I want to count trees per species, i.e. \>=100mm, this tree would
> also be counted by your current function, is this correct?

Counting trees by species.

``` r
# This preserves missing `dbh` (unless `na.rm = FALSE`)
trees <- census %>% 
  pick_dbh_min(100)
trees
#>   dbh  sp treeID stemID
#> 2 120 sp1      1    1.2
#> 5  NA sp2      2    2.3

# Count unique instances of treeID by species
trees %>% 
  group_by(sp) %>% 
  abundance_tree()
#> # A tibble: 2 x 2
#>   sp        n
#>   <chr> <int>
#> 1 sp1       1
#> 2 sp2       1
```

> If so, this will be wrong, since it will be counted twice. A tree with
> stems 120 and 20 is a tree, not a sapling, and should only be counted
> as a tree.

You are right. Thanks for your example and expectation. Now I see the
problem. The good solution seems to be a specialized function that
collapses a table of potentially multi-stem trees to a table of one stem
per tree, where the stem that remains is that of maximum `dbh`. The
function may look someting like this:

``` r
collapse_dbh_max <- function(x) {
  x %>% 
    group_by(treeID) %>% 
    arrange(desc(dbh)) %>% 
    filter(row_number() == 1) %>% 
    ungroup()
}
```

Now, we start by collapsing to maximun dbh and then we repeat the
process/

``` r
saplings <- census %>% 
  collapse_dbh_max() %>% 
  filter(dbh >= 10) %>% 
  filter(dbh < 100)
#> Warning in filter_impl(.data, quo): hybrid evaluation forced for
#> `row_number`. Please use dplyr::row_number() or library(dplyr) to remove
#> this warning.

#> Warning in filter_impl(.data, quo): hybrid evaluation forced for
#> `row_number`. Please use dplyr::row_number() or library(dplyr) to remove
#> this warning.
saplings
#> # A tibble: 1 x 4
#>     dbh sp    treeID stemID
#>   <dbl> <chr> <chr>  <chr> 
#> 1    99 sp2   2      2.2

saplings %>% 
  group_by(sp) %>% 
  abundance_tree()
#> # A tibble: 1 x 2
#>   sp        n
#>   <chr> <int>
#> 1 sp2       1
```

``` r
trees <- census %>% 
  collapse_dbh_max() %>% 
  filter(dbh >= 100)
#> Warning in filter_impl(.data, quo): hybrid evaluation forced for
#> `row_number`. Please use dplyr::row_number() or library(dplyr) to remove
#> this warning.

#> Warning in filter_impl(.data, quo): hybrid evaluation forced for
#> `row_number`. Please use dplyr::row_number() or library(dplyr) to remove
#> this warning.
trees
#> # A tibble: 1 x 4
#>     dbh sp    treeID stemID
#>   <dbl> <chr> <chr>  <chr> 
#> 1   120 sp1   1      1.2

trees %>% 
  group_by(sp) %>% 
  abundance_tree()
#> # A tibble: 1 x 2
#>   sp        n
#>   <chr> <int>
#> 1 sp1       1
```

Is this the expected result?

TODO:

  - fix warnings
  - wrap into `count_trees()` and `count_saplings()`, by … or
    `count_between(.data, ..., lower = 0, upper = Inf)`.
  - move this article out of tutorial into issues.
