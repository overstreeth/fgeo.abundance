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
    dbh = c(20, 120, 22, 99, NA),
    hom = c(130, 130, 130, 130, 130)
)
census
#> # A tibble: 5 x 6
#>   stringsAsFactors sp    treeID stemID   dbh   hom
#>   <lgl>            <chr> <chr>  <chr>  <dbl> <dbl>
#> 1 FALSE            sp1   1      1.1       20   130
#> 2 FALSE            sp1   1      1.2      120   130
#> 3 FALSE            sp2   2      2.1       22   130
#> 4 FALSE            sp2   2      2.2       99   130
#> 5 FALSE            sp2   2      2.3       NA   130
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
#> # A tibble: 4 x 6
#>   stringsAsFactors sp    treeID stemID   dbh   hom
#>   <lgl>            <chr> <chr>  <chr>  <dbl> <dbl>
#> 1 FALSE            sp1   1      1.1       20   130
#> 2 FALSE            sp2   2      2.1       22   130
#> 3 FALSE            sp2   2      2.2       99   130
#> 4 FALSE            sp2   2      2.3       NA   130

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
largest <- census %>% pick_largest_hom_dbh()
largest
#> # A tibble: 2 x 6
#>   stringsAsFactors sp    treeID stemID   dbh   hom
#>   <lgl>            <chr> <chr>  <chr>  <dbl> <dbl>
#> 1 FALSE            sp1   1      1.2      120   130
#> 2 FALSE            sp2   2      2.2       99   130

saplings_good <- largest %>% 
  filter(dbh >= 10) %>% 
  filter(dbh < 100)
saplings_good
#> # A tibble: 1 x 6
#>   stringsAsFactors sp    treeID stemID   dbh   hom
#>   <lgl>            <chr> <chr>  <chr>  <dbl> <dbl>
#> 1 FALSE            sp2   2      2.2       99   130
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

Above we picked saplings with the new function `pick_largest_hom_dbh()`
and `dplyr::filter()`. That is quite simple but, considering this may be
a very common task, we can make it even simpler with the new specialized
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

## Correcting for batreesses

This sections uses a different dataset that shows an additional feature
of `count_woods()` and friends, i.e. the ability to pick first the stem
of maximum `hom` and then the maximum `dbh`. This is to correct for the
effect of buttresses.

``` r
census <- tibble::tribble(
    ~sp, ~treeID, ~stemID, ~hom, ~dbh,
  "sp1",     "1",   "1.1",   10,   10,
  "sp1",     "1",   "1.2",   10,  888,
  "sp1",     "1",   "1.2",   88,  100,
  "sp2",     "2",   "2.1",   10,   22,
  "sp2",     "2",   "2.2",   10,   99,
  "sp2",     "2",   "2.2",   22,   88,
  "sp2",     "2",   "2.3",   10,   NA
)
census
#> # A tibble: 7 x 5
#>   sp    treeID stemID   hom   dbh
#>   <chr> <chr>  <chr>  <dbl> <dbl>
#> 1 sp1   1      1.1       10    10
#> 2 sp1   1      1.2       10   888
#> 3 sp1   1      1.2       88   100
#> 4 sp2   2      2.1       10    22
#> 5 sp2   2      2.2       10    99
#> 6 sp2   2      2.2       22    88
#> 7 sp2   2      2.3       10    NA
```

Notice that each `treeID` has duplicated `stemID`s, which correspond to
two measures on the same census of the same stem but at different
heights. In this case, the code will sort the data first by `hom`, then
by `dbh`, and pick the first row.

``` r
census %>% pick_largest_hom_dbh()
#> # A tibble: 2 x 5
#>   sp    treeID stemID   hom   dbh
#>   <chr> <chr>  <chr>  <dbl> <dbl>
#> 1 sp1   1      1.2       88   100
#> 2 sp2   2      2.2       22    88
```

## Creating tables of abundance and basal area

``` r
vft <- fgeo.data::luquillo_vft_4quad
one_stem_per_treeid <- vft %>% pick_largest_hom_dbh()

# Trees
(trees <- one_stem_per_treeid %>% pick_trees())
#> # A tibble: 532 x 32
#>     DBHID PlotName PlotID Family Genus SpeciesName Mnemonic Subspecies
#>     <int> <chr>     <int> <chr>  <chr> <chr>       <chr>    <chr>     
#>  1 353174 luquillo      1 Morac~ Ficus spp         FICSPP   <NA>      
#>  2 352708 luquillo      1 Areca~ Roys~ borinquena  ROYBOR   <NA>      
#>  3 352717 luquillo      1 Fabac~ Inga  laurina     INGLAU   <NA>      
#>  4 353153 luquillo      1 Fabac~ Inga  laurina     INGLAU   <NA>      
#>  5 384554 luquillo      1 Urtic~ Cecr~ schreberia~ CECSCH   <NA>      
#>  6 395112 luquillo      1 Urtic~ Cecr~ schreberia~ CECSCH   <NA>      
#>  7 352695 luquillo      1 Urtic~ Cecr~ schreberia~ CECSCH   <NA>      
#>  8 388056 luquillo      1 Urtic~ Cecr~ schreberia~ CECSCH   <NA>      
#>  9 385118 luquillo      1 Urtic~ Cecr~ schreberia~ CECSCH   <NA>      
#> 10 352690 luquillo      1 Salic~ Laet~ procera     LAEPRO   <NA>      
#> # ... with 522 more rows, and 24 more variables: SpeciesID <int>,
#> #   SubspeciesID <chr>, QuadratName <chr>, QuadratID <int>, PX <dbl>,
#> #   PY <dbl>, QX <dbl>, QY <dbl>, TreeID <int>, Tag <chr>, StemID <int>,
#> #   StemNumber <int>, StemTag <int>, PrimaryStem <chr>, CensusID <int>,
#> #   PlotCensusNumber <int>, DBH <dbl>, HOM <dbl>, ExactDate <date>,
#> #   Date <int>, ListOfTSM <chr>, HighHOM <int>, LargeStem <chr>,
#> #   Status <chr>

trees %>% abundance_byyr()
#> Warning: No observation has .status = dead
#>   * Detected values: alive

#> Warning: No observation has .status = dead
#>   * Detected values: alive
#> # A tibble: 23 x 5
#>    species                  Family         `2006` `2012` `2016`
#>    <chr>                    <chr>           <dbl>  <dbl>  <dbl>
#>  1 Alchornea latifolia      Euphorbiaceae       2      1      1
#>  2 Alchorneopsis floribunda Euphorbiaceae       2      4      4
#>  3 Buchenavia tetraphylla   Combretaceae        0      1      1
#>  4 Casearia arborea         Salicaceae         12     15     18
#>  5 Cecropia schreberiana    Urticaceae         59     45     19
#>  6 Cordia sulcata           Boraginaceae        2      2      2
#>  7 Dacryodes excelsa        Burseraceae         1      1      1
#>  8 Drypetes glauca          Putranjivaceae      1      1      1
#>  9 Ficus spp                Moraceae            1      1      1
#> 10 Guarea guidonia          Meliaceae           1      2      2
#> # ... with 13 more rows

trees %>% basal_area_byyr()
#> Warning: No observation has .status = dead
#>   * Detected values: alive

#> Warning: No observation has .status = dead
#>   * Detected values: alive
#> # A tibble: 23 x 5
#>    species                  Family           `2006`   `2012`  `2016`
#>    <chr>                    <chr>             <dbl>    <dbl>   <dbl>
#>  1 Alchornea latifolia      Euphorbiaceae    40718.   25447.  29559.
#>  2 Alchorneopsis floribunda Euphorbiaceae    27587.   99340. 174959.
#>  3 Buchenavia tetraphylla   Combretaceae         0     9161.  11310.
#>  4 Casearia arborea         Salicaceae      168141.  235160. 302074.
#>  5 Cecropia schreberiana    Urticaceae     2099660. 1737494. 977482.
#>  6 Cordia sulcata           Boraginaceae     37443.   39296.  42016.
#>  7 Dacryodes excelsa        Burseraceae      22432.   44863.  59396.
#>  8 Drypetes glauca          Putranjivaceae   10936.   11310.  11310.
#>  9 Ficus spp                Moraceae        223123.  209117. 229871.
#> 10 Guarea guidonia          Meliaceae        14957.   43832.  55428.
#> # ... with 13 more rows

# Saplings
(saplings <- one_stem_per_treeid %>% pick_saplings())
#> # A tibble: 1,009 x 32
#>     DBHID PlotName PlotID Family Genus SpeciesName Mnemonic Subspecies
#>     <int> <chr>     <int> <chr>  <chr> <chr>       <chr>    <chr>     
#>  1 385121 luquillo      1 Arali~ Sche~ morototoni  SCHMOR   <NA>      
#>  2 388199 luquillo      1 Urtic~ Cecr~ schreberia~ CECSCH   <NA>      
#>  3 422483 luquillo      1 Areca~ Pres~ acuminata   PREMON   <NA>      
#>  4 423334 luquillo      1 Areca~ Pres~ acuminata   PREMON   <NA>      
#>  5 388416 luquillo      1 Combr~ Buch~ tetraphylla BUCTET   <NA>      
#>  6 423335 luquillo      1 Areca~ Pres~ acuminata   PREMON   <NA>      
#>  7 384613 luquillo      1 Arali~ Sche~ morototoni  SCHMOR   <NA>      
#>  8 388515 luquillo      1 Salic~ Case~ arborea     CASARB   <NA>      
#>  9 388591 luquillo      1 Salic~ Case~ arborea     CASARB   <NA>      
#> 10 385127 luquillo      1 Arali~ Sche~ morototoni  SCHMOR   <NA>      
#> # ... with 999 more rows, and 24 more variables: SpeciesID <int>,
#> #   SubspeciesID <chr>, QuadratName <chr>, QuadratID <int>, PX <dbl>,
#> #   PY <dbl>, QX <dbl>, QY <dbl>, TreeID <int>, Tag <chr>, StemID <int>,
#> #   StemNumber <int>, StemTag <int>, PrimaryStem <chr>, CensusID <int>,
#> #   PlotCensusNumber <int>, DBH <dbl>, HOM <dbl>, ExactDate <date>,
#> #   Date <int>, ListOfTSM <chr>, HighHOM <int>, LargeStem <chr>,
#> #   Status <chr>

saplings %>% abundance_byyr()
#> Warning: No observation has .status = dead
#>   * Detected values: alive

#> Warning: No observation has .status = dead
#>   * Detected values: alive
#> # A tibble: 42 x 5
#>    species                  Family        `2006` `2012` `2016`
#>    <chr>                    <chr>          <dbl>  <dbl>  <dbl>
#>  1 Alchornea latifolia      Euphorbiaceae      3      1      0
#>  2 Alchorneopsis floribunda Euphorbiaceae      4      0      0
#>  3 Buchenavia tetraphylla   Combretaceae       2      1      1
#>  4 Byrsonima spicata        Malpighiaceae      3      3      2
#>  5 Casearia arborea         Salicaceae        92     86     87
#>  6 Casearia sylvestris      Salicaceae        13      8      6
#>  7 Cecropia schreberiana    Urticaceae         4      0      0
#>  8 Chionanthus domingensis  Oleaceae           0      0      1
#>  9 Cordia borinquensis      Boraginaceae       8      6      7
#> 10 Dacryodes excelsa        Burseraceae        1      1      1
#> # ... with 32 more rows

saplings %>% basal_area_byyr()
#> Warning: No observation has .status = dead
#>   * Detected values: alive

#> Warning: No observation has .status = dead
#>   * Detected values: alive
#> # A tibble: 42 x 5
#>    species                  Family         `2006`  `2012`  `2016`
#>    <chr>                    <chr>           <dbl>   <dbl>   <dbl>
#>  1 Alchornea latifolia      Euphorbiaceae   3071.    460.      0 
#>  2 Alchorneopsis floribunda Euphorbiaceae  10647.      0       0 
#>  3 Buchenavia tetraphylla   Combretaceae    8258.   1320.   1340.
#>  4 Byrsonima spicata        Malpighiaceae   4449.   6367.   5166.
#>  5 Casearia arborea         Salicaceae    108017. 127258. 146016.
#>  6 Casearia sylvestris      Salicaceae     11386.  10387.   8344.
#>  7 Cecropia schreberiana    Urticaceae     17836.      0       0 
#>  8 Chionanthus domingensis  Oleaceae           0       0     115.
#>  9 Cordia borinquensis      Boraginaceae   16577.  11214.  11640.
#> 10 Dacryodes excelsa        Burseraceae     2124.   2376.   3217.
#> # ... with 32 more rows
```

## TODO

Think more about status. Now, the process by which dead trees are
excluded is too obscure.
