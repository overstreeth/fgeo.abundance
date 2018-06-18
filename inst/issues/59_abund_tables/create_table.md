---
title: "Tables of abundance and basal area"
subtitle: BCI, Sherman and Cocoli
author: "Mauro Lepore"
date: "2018-06-18"
output:
  html_document:
    toc: yes
    toc_depth: '3'
    theme: united
    keep_md: true
---



Setup.


```r
library(fgeo.tool)
library(fgeo.base)
library(fgeo.abundance)
```

To create tables of abundance and basal area first pick the data you want. Your code should look something like this:

```R
pick1 <- pick_plotname(VIEWFULLTABLE, "PLOTNAME")
pick2 <- drop_dead_trees_by_cns(pick1)
pick3 <- pick_dbh_min(pick2, MINIMUM_DBH)
```

Then you can create the tables with:

```R
abundance <- byyr_abundance(pick3)
basal_area <- byyr_basal_area(pick3)
```

To standardize the table of basal area use something like this:

```R
years <- setdiff(names(basal_area), c("species", "Family"))
in_sq_m <- conv_unit_at(basal_area, .at = years, from = "mm2", to = "m2")
basal_area_in_he <- standardize_at(in_sq_m, .at = years, total = TOTAL_HECTARES)
```

Next, I do this for three plots: Sherman, Cocoli and BCI. To avoid duplication I first write some disposable utility-funcitons.


```r
# Pick specific data from a ViewFullTable.
pick_vft <- function(vft, plot_nm) {
  pick1 <- fgeo.base::pick_plotname(vft = vft, plot_nm = plot_nm)
  pick2 <- drop_dead_trees_by_cns(pick1)
  pick_dbh_min(pick2, 10)
}

# Standardize basal area by total plot-hectares.
standardize_ba <- function(ba, total) {
  years <- setdiff(names(ba), c("species", "Family"))
  in_he <- conv_unit_at(ba, .at = years, from = "mm2", to = "m2")
  standardize_at(in_he, .at = years, total = total)
}

path_iss59 <- function(path) {
  here::here("inst/issues/59_abund_tables", path)
}

write_tables <- function(vft, plot_nm, total) {
  pick <- pick_vft(vft = vft, plot_nm = plot_nm)
  
  abun <- byyr_abundance(pick)
  readr::write_csv(abun, path_iss59(paste0("tbl/", plot_nm, "_abundance.csv")))
  
  ba <- standardize_ba(byyr_basal_area(pick), total = total)
  readr::write_csv(ba, path_iss59(paste0("tbl/", plot_nm, "_basal_area.csv")))
}
```

Now I'm ready to read the data and crete the tables -- which I save locally:

* Read ViewFullTables.


```r
# BCI
path_bci <- here::here("inst/issues/59_abund_tables/vft_bci.csv")
vft_bci <- readr::read_csv(path_bci)
#> Parsed with column specification:
#> cols(
#>   .default = col_character(),
#>   DBHID = col_integer(),
#>   PlotID = col_integer(),
#>   SpeciesID = col_integer(),
#>   SubspeciesID = col_integer(),
#>   QuadratID = col_integer(),
#>   PX = col_double(),
#>   PY = col_double(),
#>   QX = col_double(),
#>   QY = col_double(),
#>   TreeID = col_integer(),
#>   StemID = col_integer(),
#>   StemNumber = col_integer(),
#>   CensusID = col_integer(),
#>   PlotCensusNumber = col_integer(),
#>   DBH = col_double(),
#>   HOM = col_double(),
#>   ExactDate = col_date(format = ""),
#>   Date = col_integer(),
#>   HighHOM = col_integer()
#> )
#> See spec(...) for full column specifications.

# Sherman and Cocoli
path_sc <- here::here("inst/issues/59_abund_tables/vft_sc.csv")
vft_sc <- readr::read_csv(path_sc)
#> Parsed with column specification:
#> cols(
#>   .default = col_integer(),
#>   PlotName = col_character(),
#>   Family = col_character(),
#>   Genus = col_character(),
#>   SpeciesName = col_character(),
#>   Mnemonic = col_character(),
#>   Subspecies = col_character(),
#>   QuadratName = col_character(),
#>   PX = col_double(),
#>   PY = col_double(),
#>   QX = col_double(),
#>   QY = col_double(),
#>   Tag = col_character(),
#>   PrimaryStem = col_character(),
#>   HOM = col_double(),
#>   ExactDate = col_date(format = ""),
#>   ListOfTSM = col_character(),
#>   LargeStem = col_character(),
#>   Status = col_character()
#> )
#> See spec(...) for full column specifications.
```

* Write tables of abundance and basal area.


```r
write_tables(vft_sc, plot_nm = "sherman", total = 5.96)
#> Using: sherman.
#> Unique values of column `Status` and argument `valid_status` should match:
#> * Status col: alive, broken below, dead, missing.
#> * valid_status arg: alive, broken below, dead, missing, stem dead.
#> Fixing status automatically.
#> Calculating tree-status (from stem `Status`) by `PlotCensusNumber`.
#> Dropping rows where `Status = dead`.
write_tables(vft_sc, plot_nm = "cocoli", total = 4)
#> Using: cocoli.
#> Unique values of column `Status` and argument `valid_status` should match:
#> * Status col: alive, broken below, dead, missing.
#> * valid_status arg: alive, broken below, dead, missing, stem dead.
#> Fixing status automatically.
#> Calculating tree-status (from stem `Status`) by `PlotCensusNumber`.
#> Dropping rows where `Status = dead`.
write_tables(vft_bci, plot_nm = "bci", total = 50)
#> Using: bci.
#> Unique values of column `Status` and argument `valid_status` should match:
#> * Status col: alive, broken below, dead, missing.
#> * valid_status arg: alive, broken below, dead, missing, stem dead.
#> Fixing status automatically.
#> Calculating tree-status (from stem `Status`) by `PlotCensusNumber`.
#> Dropping rows where `Status = dead`.
#> Warning in drop_if_missing_dates(.): Detected and ignoring missing dates.

#> Warning in drop_if_missing_dates(.): Detected and ignoring missing dates.
```

Les't visualize the output.


```r
fgeo.tool::csv_to_df_lst(here::here("inst/issues/59_abund_tables/tbl"))
#> $bci_abundance.csv
#> # A tibble: 325 x 10
#>    species  Family `1982` `1985` `1990` `1995` `2000` `2005` `2010` `2015`
#>    <chr>    <chr>   <int>  <dbl>  <int>  <int>  <int>  <int>  <int>  <int>
#>  1 Abarema~ Fabac~     10     10     11     12     12     16     33     47
#>  2 Acacia ~ Fabac~      6      8     14     14     11     24     55     55
#>  3 Acalyph~ Eupho~   1894   1603   1413    906    850   1243   1738   2809
#>  4 Acalyph~ Eupho~    115     98     81     64     71    107     92    113
#>  5 Adelia ~ Eupho~    490    430    736    581    569    519    532    594
#>  6 Aegiphi~ Lamia~    142    131    101     83     63     47     40     26
#>  7 Alchorn~ Eupho~    411    352    411    347    318    326    428    447
#>  8 Alchorn~ Eupho~      3      3      5      3      2      2      1      0
#>  9 Alibert~ Rubia~    363    420    506    509    481    497    576    637
#> 10 Allophy~ Sapin~    204    208    268    216    179    167    192    216
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
#>  5 Adelia ~ Eupho~ 6.92e-2 6.94e-2 6.99e-2 6.16e-2 5.88e-2 5.03e-2 5.04e-2
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
#>  2 Acalypha diversifolia   Euphorbiaceae            24     32     31
#>  3 Acalypha macrostachya   Euphorbiaceae             1      2      3
#>  4 Adelia triloba          Euphorbiaceae             5      7      7
#>  5 Aegiphila panamensis    Lamiaceae                 2      1      1
#>  6 Albizia adinocephala    Fabaceae-mimosoideae     56     55     61
#>  7 Albizia procera         Fabaceae-mimosoideae      2      3      3
#>  8 Alchornea costaricensis Euphorbiaceae             2      1      1
#>  9 Alibertia edulis        Rubiaceae               340    311    312
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
#>  3 Acalypha diversifolia   Euphorbiace~     12     10     12     34     41
#>  4 Aegiphila panamensis    Lamiaceae         1      0      0      0      0
#>  5 Alchornea costaricensis Euphorbiace~      1      1      1      1      1
#>  6 Alchornea latifolia     Euphorbiace~    140    122    124    122    108
#>  7 Alibertia edulis        Rubiaceae         2      2      2      2      1
#>  8 Amaioua corymbosa       Rubiaceae        93     90     92     89     96
#>  9 Andira inermis          Fabaceae-pa~     41     39     40     40     37
#> 10 Annona spraguei         Annonaceae       15      9      7     13     13
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


