Translating SQL to R
================

> When I run the abundance query on ViewFulltable in SQL, I get almost
> the same numbers as Rick’s
tables:

``` sql
SELECT mnemonic,plotcensusnumber,COUNT(distinct treeid) AS cnt from ViewFullTable WHERE dbh>=10  and plotid=1 GROUP BY mnemonic, plotcensusnumber;
```

– Suzanne

Here is a translation in R:

  - Setup.

<!-- end list -->

``` r
library(tidyverse)
library(fgeo)

path <- here::here("inst/issues/59_abund_tables/vft_bci.csv")
ViewFullTable <- read_vft(path, delim = ",")
```

  - Translation.

<!-- end list -->

``` r
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

  - Reshaping the result for a layout similar to Rick’s
    ([example](https://forestgeo.si.edu/bci-abundance-all-tree-species-50-ha-plot-1982-2005-trees)).

<!-- end list -->

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

  - Saving to share with Suzanne.

<!-- end list -->

``` r
write_csv(result_wide, "bci_sql_to_r.csv")
```
