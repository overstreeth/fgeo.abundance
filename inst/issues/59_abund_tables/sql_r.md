Translating SQL to R
================

``` sql
SELECT mnemonic,plotcensusnumber,COUNT(distinct treeid) AS cnt from ViewFullTable WHERE dbh>=10  and plotid=1 GROUP BY mnemonic, plotcensusnumber;
```

``` r
library(tidyverse)
```

    ## -- Attaching packages ---------------------------------------------- tidyverse 1.2.1 --

    ## v ggplot2 3.0.0     v purrr   0.2.5
    ## v tibble  1.4.2     v dplyr   0.7.6
    ## v tidyr   0.8.1     v stringr 1.3.1
    ## v readr   1.1.1     v forcats 0.3.0

    ## -- Conflicts ------------------------------------------------- tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(fgeo)
```

    ## -- Attaching packages ---------------------------------------------- fgeo 0.0.0.9000 --

    ## v fgeo.abundance  0.0.0.9004     v fgeo.demography 0.0.0.9000
    ## v fgeo.base       0.0.0.9001     v fgeo.habitat    0.0.0.9006
    ## v fgeo.data       0.0.0.9002     v fgeo.map        0.0.0.9204
    ## v fgeo.abundance  0.0.0.9004     v fgeo.tool       0.0.0.9003

    ## -- Conflicts ------------------------------------------------------ fgeo_conflicts() --
    ## x fgeo.tool::filter() masks dplyr::filter(), stats::filter()
    ## x dplyr::intersect()  masks base::intersect()
    ## x dplyr::lag()        masks stats::lag()
    ## x dplyr::setdiff()    masks base::setdiff()
    ## x dplyr::setequal()   masks base::setequal()
    ## x dplyr::union()      masks base::union()

``` r
path <- here::here("inst/issues/59_abund_tables/vft_bci.csv")
ViewFullTable <- read_vft(path, delim = ",")

result <- ViewFullTable %>% 
  filter(DBH > 10 & PlotID == 1) %>% 
  select(Mnemonic, PlotCensusNumber, TreeID) %>% 
  group_by(Mnemonic, PlotCensusNumber) %>% 
  summarize(cnt = n_distinct(TreeID))
result
```

    ## # A tibble: 2,411 x 3
    ## # Groups:   Mnemonic [?]
    ##    Mnemonic PlotCensusNumber   cnt
    ##    <chr>               <int> <int>
    ##  1 acacme                  1     5
    ##  2 acacme                  2     5
    ##  3 acacme                  3    11
    ##  4 acacme                  4    12
    ##  5 acacme                  5    10
    ##  6 acacme                  6    21
    ##  7 acacme                  7    46
    ##  8 acacme                  8    40
    ##  9 acaldi                  1  1278
    ## 10 acaldi                  2  1023
    ## # ... with 2,401 more rows

For a layout similar to Rick’s
([example](https://forestgeo.si.edu/bci-abundance-all-tree-species-50-ha-plot-1982-2005-trees)).

``` r
result_wide <- spread(result, PlotCensusNumber, value = "cnt") %>% 
  arrange(Mnemonic)
result_wide
```

    ## # A tibble: 325 x 9
    ## # Groups:   Mnemonic [325]
    ##    Mnemonic   `1`   `2`   `3`   `4`   `5`   `6`   `7`   `8`
    ##    <chr>    <int> <int> <int> <int> <int> <int> <int> <int>
    ##  1 acacme       5     5    11    12    10    21    46    40
    ##  2 acaldi    1278  1023   803   511   480   710   999  1392
    ##  3 acalma      74    62    44    42    43    51    52    61
    ##  4 ade1tr     312   291   277   216   161   143   143   120
    ##  5 aegipa     125   113    92    77    62    44    40    25
    ##  6 alchco     367   304   266   227   228   226   316   263
    ##  7 alchla       2     2     3     2     1     1     1    NA
    ##  8 alibed     220   237   366   366   349   356   403   424
    ##  9 allops     122   119   150   123   111   100   108    96
    ## 10 alsebl    5219  5368  7991  7690  7571  7371  7592  7717
    ## # ... with 315 more rows

``` r
write_csv(result_wide, "bci_sql_to_r.csv")
```
