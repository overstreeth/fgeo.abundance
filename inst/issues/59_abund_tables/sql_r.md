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
  filter(DBH >= 10 & PlotID == 1) %>% 
  select(Mnemonic, PlotCensusNumber, TreeID) %>% 
  group_by(Mnemonic, PlotCensusNumber) %>% 
  summarize(cnt = n_distinct(TreeID))
result
```

    ## # A tibble: 2,424 x 3
    ## # Groups:   Mnemonic [?]
    ##    Mnemonic PlotCensusNumber   cnt
    ##    <chr>               <int> <int>
    ##  1 acacme                  1     5
    ##  2 acacme                  2     6
    ##  3 acacme                  3    11
    ##  4 acacme                  4    12
    ##  5 acacme                  5    10
    ##  6 acacme                  6    21
    ##  7 acacme                  7    47
    ##  8 acacme                  8    41
    ##  9 acaldi                  1  1562
    ## 10 acaldi                  2  1200
    ## # ... with 2,414 more rows

  - Reshaping the result for a layout similar to Rick’s
    ([example](https://forestgeo.si.edu/bci-abundance-all-tree-species-50-ha-plot-1982-2005-trees)).

<!-- end list -->

``` r
result_wide <- spread(result, PlotCensusNumber, value = "cnt") %>% 
  arrange(Mnemonic)
result_wide
```

    ## # A tibble: 326 x 9
    ## # Groups:   Mnemonic [326]
    ##    Mnemonic   `1`   `2`   `3`   `4`   `5`   `6`   `7`   `8`
    ##    <chr>    <int> <int> <int> <int> <int> <int> <int> <int>
    ##  1 acacme       5     6    11    12    10    21    47    41
    ##  2 acaldi    1562  1200   817   523   488   741  1019  1439
    ##  3 acalma      79    66    44    42    43    52    52    62
    ##  4 ade1tr     346   315   280   219   163   143   143   120
    ##  5 aegipa     136   126    92    77    62    44    40    26
    ##  6 alchco     385   314   266   228   229   229   319   266
    ##  7 alchla       2     2     3     2     1     1     1    NA
    ##  8 alibed     304   342   378   379   357   372   417   455
    ##  9 allops     175   171   153   123   112   103   112    97
    ## 10 alsebl    7598  8050  8412  8175  7869  7752  7928  8064
    ## # ... with 316 more rows

  - Saving to share with Suzanne.

<!-- end list -->

``` r
write_csv(result_wide, "bci_sql_to_r.csv")
```
