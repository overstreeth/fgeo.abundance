---
title: "Tables of abundance and basal area"
subtitle: BCI, Sherman and Cocoli
author: "Mauro Lepore"
date: "2018-07-02"
output:
  html_document:
    toc: yes
    toc_depth: '3'
    theme: united
    keep_md: true
---



Setup.


```r
library(tidyverse)
library(fgeo)
```

To create tables of abundance and basal area first pick the data you want. Your code should look something like this:

```R
pick1 <- pick_plotname(VIEWFULLTABLE, "PLOTNAME")
pick2 <- pick_dbh_min(pick1, MINIMUM_DBH)
```

Then you can create the tables with:

```R
abundance <- abundance_byyr(pick2)
basal_area <- basal_area_byyr(pick2)
```

To standardize the table of basal area use something like this:

```R
years <- setdiff(names(basal_area), c("species", "Family"))
in_sq_m <- convert_unit_at(basal_area, .at = years, from = "mm2", to = "m2")
basal_area_in_he <- standardize_at(in_sq_m, .at = years, denominator = denominator_HECTARES)
```

Next, I do this for three plots: Sherman, Cocoli and BCI. To avoid duplication I first write some disposable utility-funcitons.


```r
# Pick specific data from a ViewFullTable.
pick_vft <- function(vft, plot_nm) {
  pick <- pick_plotname(vft, plot_nm)
  pick_dbh_min(pick, 10)
}

# Standardize basal area by total plot-hectares.
standardize_ba <- function(ba, denominator) {
  years <- setdiff(names(ba), c("species", "Family"))
  in_he <- convert_unit_at(ba, .at = years, from = "mm2", to = "m2")
  standardize_at(in_he, years, denominator)
}

path_iss59 <- function(path) {
  here::here("inst/issues/59_abund_tables", path)
}

write_tables <- function(vft, plot_nm, denominator) {
  pick <- pick_vft(vft, plot_nm)
  
  abun <- abundance_byyr(pick)
  readr::write_csv(abun, path_iss59(paste0("tbl/", plot_nm, "_abundance.csv")))
  
  ba <- standardize_ba(basal_area_byyr(pick), denominator)
  readr::write_csv(ba, path_iss59(paste0("tbl/", plot_nm, "_basal_area.csv")))
}
```

Now I'm ready to read the data and crete the tables -- which I save locally:

* Read ViewFullTables.


```r
# BCI
path_bci <- here::here("inst/issues/59_abund_tables/vft_bci.csv")
vft_bci <- readr::read_csv(path_bci)

# Sherman and Cocoli
path_sc <- here::here("inst/issues/59_abund_tables/vft_sc.csv")
vft_sc <- readr::read_csv(path_sc)
```

* Write tables of abundance and basal area.


```r
write_tables(vft_sc, "sherman", denominator = 5.96)
write_tables(vft_sc, "cocoli", denominator = 4)
write_tables(vft_bci, "bci", denominator = 50)
```

Les't visualize the output.


```r
folder <- here::here("inst/issues/59_abund_tables/tbl")
dfs <- csv_to_dfs(folder)
dfs
#> $bci_abundance.csv
#> # A tibble: 325 x 10
#>    species  Family `1982` `1985` `1990` `1995` `2000` `2005` `2010` `2015`
#>    <chr>    <chr>   <int>  <int>  <int>  <int>  <int>  <int>  <int>  <int>
#>  1 Abarema~ Fabac~     10     10     11     12     12     16     32     44
#>  2 Acacia ~ Fabac~      5      7     11     12     10     21     48     44
#>  3 Acalyph~ Eupho~   1562   1273   1072    685    576    850   1142   1605
#>  4 Acalyph~ Eupho~     79     68     48     42     43     52     53     72
#>  5 Adelia ~ Eupho~    346    321    290    227    167    145    146    133
#>  6 Aegiphi~ Lamia~    136    129     99     80     62     46     40     26
#>  7 Alchorn~ Eupho~    385    315    271    230    231    230    320    274
#>  8 Alchorn~ Eupho~      2      2      3      2      2      2      1      0
#>  9 Alibert~ Rubia~    304    345    393    399    378    394    450    479
#> 10 Allophy~ Sapin~    175    176    164    131    114    105    116    104
#> # ... with 315 more rows
#> 
#> $bci_basal_area.csv
#> # A tibble: 325 x 10
#>    species  Family  `1982`  `1985`  `1990`  `1995`  `2000`  `2005`  `2010`
#>    <chr>    <chr>    <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>
#>  1 Abarema~ Fabac~ 2.51e-3 4.08e-3 6.26e-3 8.44e-3 1.08e-2 1.29e-2 1.53e-2
#>  2 Acacia ~ Fabac~ 9.22e-4 6.41e-4 8.20e-4 1.22e-3 6.10e-4 8.67e-4 1.88e-3
#>  3 Acalyph~ Eupho~ 2.06e-2 2.15e-2 1.83e-2 1.18e-2 1.04e-2 1.23e-2 1.78e-2
#>  4 Acalyph~ Eupho~ 3.93e-3 2.89e-3 2.67e-3 1.75e-3 1.91e-3 2.38e-3 2.50e-3
#>  5 Adelia ~ Eupho~ 6.92e-2 6.94e-2 7.01e-2 6.16e-2 5.88e-2 5.03e-2 5.04e-2
#>  6 Aegiphi~ Lamia~ 1.08e-2 1.04e-2 1.04e-2 1.01e-2 9.41e-3 7.58e-3 6.11e-3
#>  7 Alchorn~ Eupho~ 2.63e-1 2.75e-1 3.48e-1 2.73e-1 2.32e-1 2.47e-1 2.36e-1
#>  8 Alchorn~ Eupho~ 8.81e-4 9.39e-4 9.95e-4 4.71e-4 6.73e-4 8.02e-4 9.66e-4
#>  9 Alibert~ Rubia~ 4.37e-3 4.75e-3 6.36e-3 6.23e-3 6.09e-3 6.22e-3 6.44e-3
#> 10 Allophy~ Sapin~ 1.73e-2 1.82e-2 1.71e-2 1.70e-2 1.55e-2 1.78e-2 1.91e-2
#> # ... with 315 more rows, and 1 more variable: `2015` <dbl>
#> 
#> $cocoli_abundance.csv
#> # A tibble: 175 x 5
#>    species                 Family               `1994` `1997` `1998`
#>    <chr>                   <chr>                 <int>  <int>  <int>
#>  1 Acacia melanoceras      Fabaceae-mimosoideae     23     31     31
#>  2 Acalypha diversifolia   Euphorbiaceae            14     18     23
#>  3 Acalypha macrostachya   Euphorbiaceae             1      2      2
#>  4 Adelia triloba          Euphorbiaceae             5      6      6
#>  5 Aegiphila panamensis    Lamiaceae                 2      1      1
#>  6 Albizia adinocephala    Fabaceae-mimosoideae     55     57     61
#>  7 Albizia procera         Fabaceae-mimosoideae      2      2      2
#>  8 Alchornea costaricensis Euphorbiaceae             2      1      1
#>  9 Alibertia edulis        Rubiaceae               253    236    230
#> 10 Alseis blackiana        Rubiaceae                 4      4      4
#> # ... with 165 more rows
#> 
#> $cocoli_basal_area.csv
#> # A tibble: 175 x 5
#>    species                 Family                  `1994`   `1997`  `1998`
#>    <chr>                   <chr>                    <dbl>    <dbl>   <dbl>
#>  1 Acacia melanoceras      Fabaceae-mimosoideae 0.00200    1.25e-3 1.29e-3
#>  2 Acalypha diversifolia   Euphorbiaceae        0.00352    4.36e-3 4.57e-3
#>  3 Acalypha macrostachya   Euphorbiaceae        0.0000283  7.64e-5 9.60e-5
#>  4 Adelia triloba          Euphorbiaceae        0.00764    8.36e-3 8.62e-3
#>  5 Aegiphila panamensis    Lamiaceae            0.000671   3.85e-5 3.85e-5
#>  6 Albizia adinocephala    Fabaceae-mimosoideae 0.0857     6.84e-2 6.43e-2
#>  7 Albizia procera         Fabaceae-mimosoideae 0.000228   4.48e-4 5.22e-4
#>  8 Alchornea costaricensis Euphorbiaceae        0.00241    2.83e-3 2.87e-3
#>  9 Alibertia edulis        Rubiaceae            0.0499     4.80e-2 4.73e-2
#> 10 Alseis blackiana        Rubiaceae            0.0234     2.49e-2 2.56e-2
#> # ... with 165 more rows
#> 
#> $sherman_abundance.csv
#> # A tibble: 274 x 7
#>    species                 Family       `1996` `1997` `1999` `2009` `2016`
#>    <chr>                   <chr>         <int>  <int>  <int>  <int>  <int>
#>  1 Abarema barbouriana     Fabaceae-mi~     10     12     12     10     10
#>  2 Acacia melanoceras      Fabaceae-mi~      1      1      1      1      0
#>  3 Acalypha diversifolia   Euphorbiace~      9      7      7     20     15
#>  4 Aegiphila panamensis    Lamiaceae         1      0      0      0      0
#>  5 Alchornea costaricensis Euphorbiace~      1      1      1      1      1
#>  6 Alchornea latifolia     Euphorbiace~     92     91     94     98     84
#>  7 Alibertia edulis        Rubiaceae         2      2      2      2      1
#>  8 Amaioua corymbosa       Rubiaceae        89     88     89     85     90
#>  9 Andira inermis          Fabaceae-pa~     41     39     39     38     33
#> 10 Annona spraguei         Annonaceae       15      9      7     12     13
#> # ... with 264 more rows
#> 
#> $sherman_basal_area.csv
#> # A tibble: 274 x 7
#>    species                 Family   `1996`  `1997`  `1999`  `2009`  `2016`
#>    <chr>                   <chr>     <dbl>   <dbl>   <dbl>   <dbl>   <dbl>
#>  1 Abarema barbouriana     Fabace~ 2.06e-2 2.25e-2 2.47e-2 0.0515  0.0748 
#>  2 Acacia melanoceras      Fabace~ 4.27e-5 5.27e-5 0.      0       0      
#>  3 Acalypha diversifolia   Euphor~ 1.10e-3 4.51e-4 4.34e-4 0.00177 0.00208
#>  4 Aegiphila panamensis    Lamiac~ 1.90e-4 0.      0.      0       0      
#>  5 Alchornea costaricensis Euphor~ 4.91e-3 5.01e-3 4.96e-3 0.00543 0.00543
#>  6 Alchornea latifolia     Euphor~ 6.35e-2 6.03e-2 6.12e-2 0.0600  0.0537 
#>  7 Alibertia edulis        Rubiac~ 1.39e-4 5.71e-5 5.71e-5 0       0      
#>  8 Amaioua corymbosa       Rubiac~ 3.27e-2 3.30e-2 3.33e-2 0.0315  0.0344 
#>  9 Andira inermis          Fabace~ 6.05e-2 6.26e-2 6.39e-2 0.0530  0.0327 
#> 10 Annona spraguei         Annona~ 1.78e-2 1.61e-2 4.59e-3 0.00171 0.00195
#> # ... with 264 more rows
```

For comparison see: 

Compare with:

* Saplings and trees: https://forestgeo.si.edu/bci-abundance-all-tree-species-50-ha-plot-1982-2005-saplings-and-trees

* Trees: https://forestgeo.si.edu/bci-abundance-all-tree-species-50-ha-plot-1982-2005-trees

Also:


```r
folder <- here::here("inst/issues/59_abund_tables/condit_et_al")
paths <- fs::path(folder, dir(folder))
dfs <- map(paths, read_delim, delim = "\t")
set_names(dfs, basename(paths))
#> $bciAbundTable.tsv
#> # A tibble: 328 x 11
#>    Latin Family Authority `1982` `1985` `1990` `1995` `2000` `2005` `2010`
#>    <chr> <chr>  <chr>      <int>  <int>  <int>  <int>  <int>  <int>  <int>
#>  1 Vach~ Fabac~ (Beurl.)~      5      6     11     12     10     21     47
#>  2 Acal~ Eupho~ Jacq.       1562   1200    817    523    488    741   1019
#>  3 Acal~ Eupho~ Jacq.         79     66     44     42     43     52     52
#>  4 Adel~ Eupho~ (Müll.Ar~    346    315    280    219    163    143    143
#>  5 Aegi~ Lamia~ Moldenke     136    126     92     77     62     44     40
#>  6 Alch~ Eupho~ Pax & K.~    385    314    266    228    229    229    319
#>  7 Alch~ Eupho~ Sw.            2      2      3      2      1      1      1
#>  8 Alib~ Rubia~ (Rich.) ~    304    342    378    379    357    372    417
#>  9 Allo~ Sapin~ Radlk.       175    171    153    123    112    103    112
#> 10 Alse~ Rubia~ Hemsl.      7598   8050   8412   8175   7869   7752   7928
#> # ... with 318 more rows, and 1 more variable: `2015` <int>
#> 
#> $cocoliAbundTable.tsv
#> # A tibble: 176 x 6
#>    Latin                   Family        Authority    `1994` `1997` `1998`
#>    <chr>                   <chr>         <chr>         <int>  <int>  <int>
#>  1 Acacia melanoceras      Fabaceae      Beurl.           23     19     19
#>  2 Acalypha diversifolia   Euphorbiaceae Jacq.            14     18     22
#>  3 Acalypha macrostachya   Euphorbiaceae Jacq.             1      2      2
#>  4 Adelia triloba          Euphorbiaceae (Müll.Arg.)~      5      6      6
#>  5 Aegiphila panamensis    Lamiaceae     Moldenke          2      1      1
#>  6 Albizia adinocephala    Fabaceae      (Donn. Sm.)~     55     53     57
#>  7 Albizia procera         Fabaceae      (Roxb.) Ben~      2      2      2
#>  8 Alchornea costaricensis Euphorbiaceae Pax & K. Ho~      2      1      1
#>  9 Alibertia edulis        Rubiaceae     (Rich.) A. ~    253    228    224
#> 10 Alseis blackiana        Rubiaceae     Hemsl.            4      4      4
#> # ... with 166 more rows
#> 
#> $shermanAbundTable.tsv
#> # A tibble: 270 x 7
#>    Latin                 Family    Authority   `1996` `1997` `1999` `2009`
#>    <chr>                 <chr>     <chr>        <int>  <int>  <int>  <int>
#>  1 Acacia sp.1           Fabaceae  <NA>             0      0      0      4
#>  2 Acacia melanoceras    Fabaceae  Beurl.           1      1      0      0
#>  3 Acalypha diversifolia Euphorbi~ Jacq.            9      7      5     20
#>  4 Aegiphila panamensis  Lamiaceae Moldenke         1      0      0      0
#>  5 Alchornea sp.3        Euphorbi~ <NA>            33     24     22     12
#>  6 Alchornea latifolia   Euphorbi~ Sw.             93     90     94     96
#>  7 Alibertia edulis      Rubiaceae (Rich.) A.~      2      2      2      0
#>  8 Amaioua corymbosa     Rubiaceae Kunth           89     88     89     83
#>  9 Andira inermis        Fabaceae  (W. Wright~     41     38     38     37
#> 10 Annona spraguei       Annonace~ Saff.           15      9      7     12
#> # ... with 260 more rows
```

