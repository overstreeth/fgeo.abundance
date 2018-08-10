---
title: "Tables of abundance and basal area"
subtitle: BCI, Sherman and Cocoli
author: "Mauro Lepore"
date: "2018-08-10"
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

[Notes on code development](https://github.com/forestgeo/fgeo.abundance/blob/master/inst/issues/59_abund_tables/count_samplings.md#creating-tables-of-abundance-and-basal-area).



```r
create_table <- function(vft, plotname, pick_stem_size, table_byyr) {
  vft_plot <- pick_plotname(vft, plotname)
  one_stem_per_treeid <- pick_largest_hom_dbh(vft_plot)
  stems <- pick_stem_size(one_stem_per_treeid)
  table_byyr(stems)
}

# Standardize basal area by total plot-hectares.
standardize_ba <- function(ba, denominator) {
  years <- setdiff(names(ba), c("species", "Family"))
  in_he <- convert_unit_at(ba, .at = years, from = "mm2", to = "m2")
  standardize_at(in_he, years, denominator)
}

create_all_tables <- function(vft, plotname, denominator) {
  tree_abund <- create_table(vft, plotname, pick_trees, abundance_byyr)
  sapling_abund <- create_table(vft, plotname, pick_saplings, abundance_byyr)
  tree_basal <- create_table(vft, plotname, pick_trees, basal_area_byyr) %>% 
    standardize_ba(denominator)
  sapling_basal <- create_table(vft, plotname, pick_saplings, basal_area_byyr) %>% 
    standardize_ba(denominator)
  
  list(
    tree_abundance = tree_abund,
    tree_basal_area = tree_basal,
    sapling_abundance = sapling_abund,
    sapling_basal_area = sapling_basal
  )
}

write_all_tables <- function(vft, plotname, dir, denominator) {
  tables <- create_all_tables(vft, plotname, denominator)
  fgeo.tool::dfs_to_csv(tables, dir, prefix = paste0(plotname, "_"))
}
```

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


```r
dir <- here::here("inst/issues/59_abund_tables/out")
vft <- bci <- sample_n(vft_bci, 10)
write_all_tables(bci, plotname = "bci", dir, denominator = 50)

vft <- sc <- sample_n(vft_sc, 10)
write_all_tables(sc, plotname = "sherman", dir, 5.96)
write_all_tables(sc, plotname = "cocoli", dir, denominator = 4)
```

Les't visualize the output.


```r
folder <- here::here("inst/issues/59_abund_tables/out")
dfs <- csv_to_dfs(folder)
str(dfs, give.attr = FALSE)
#> List of 12
#>  $ bci_sapling_abundance.csv     :Classes 'tbl_df', 'tbl' and 'data.frame':	6 obs. of  9 variables:
#>   ..$ species: chr [1:6] "Alseis blackiana" "Brosimum alicastrum" "Hybanthus prunifolius" "Psychotria horizontalis" ...
#>   ..$ Family : chr [1:6] "Rubiaceae" "Moraceae" "Violaceae" "Rubiaceae" ...
#>   ..$ 1982   : int [1:6] 0 0 1 1 0 0
#>   ..$ 1985   : int [1:6] 0 0 1 0 0 0
#>   ..$ 1995   : int [1:6] 0 0 1 0 0 0
#>   ..$ 2000   : int [1:6] 1 0 0 0 1 0
#>   ..$ 2005   : int [1:6] 1 0 0 0 0 0
#>   ..$ 2010   : int [1:6] 0 0 0 0 0 1
#>   ..$ 2015   : int [1:6] 0 1 0 0 0 0
#>  $ bci_sapling_basal_area.csv    :Classes 'tbl_df', 'tbl' and 'data.frame':	6 obs. of  9 variables:
#>   ..$ species: chr [1:6] "Alseis blackiana" "Brosimum alicastrum" "Hybanthus prunifolius" "Psychotria horizontalis" ...
#>   ..$ Family : chr [1:6] "Rubiaceae" "Moraceae" "Violaceae" "Rubiaceae" ...
#>   ..$ 1982   : num [1:6] 0.00 0.00 3.53e-06 1.57e-06 0.00 ...
#>   ..$ 1985   : num [1:6] 0.00 0.00 1.57e-06 0.00 0.00 ...
#>   ..$ 1995   : num [1:6] 0.00 0.00 2.26e-06 0.00 0.00 ...
#>   ..$ 2000   : num [1:6] 5.09e-06 0.00 0.00 0.00 1.90e-06 ...
#>   ..$ 2005   : num [1:6] 3.53e-06 0.00 0.00 0.00 0.00 ...
#>   ..$ 2010   : num [1:6] 0 0 0 0 0 ...
#>   ..$ 2015   : num [1:6] 0.00 2.27e-05 0.00 0.00 0.00 ...
#>  $ bci_tree_abundance.csv        :Classes 'tbl_df', 'tbl' and 'data.frame':	1 obs. of  3 variables:
#>   ..$ species: chr "Gustavia superba"
#>   ..$ Family : chr "Lecythidaceae"
#>   ..$ 1981   : int 1
#>  $ bci_tree_basal_area.csv       :Classes 'tbl_df', 'tbl' and 'data.frame':	1 obs. of  3 variables:
#>   ..$ species: chr "Gustavia superba"
#>   ..$ Family : chr "Lecythidaceae"
#>   ..$ 1981   : num 0.000982
#>  $ cocoli_sapling_abundance.csv  :Classes 'tbl_df', 'tbl' and 'data.frame':	1 obs. of  3 variables:
#>   ..$ species: chr "Eugenia principium"
#>   ..$ Family : chr "Myrtaceae"
#>   ..$ 1994   : int 1
#>  $ cocoli_sapling_basal_area.csv :Classes 'tbl_df', 'tbl' and 'data.frame':	1 obs. of  3 variables:
#>   ..$ species: chr "Eugenia principium"
#>   ..$ Family : chr "Myrtaceae"
#>   ..$ 1994   : num 1.96e-05
#>  $ cocoli_tree_abundance.csv     :Classes 'tbl_df', 'tbl' and 'data.frame':	1 obs. of  3 variables:
#>   ..$ species: chr "Trichilia pleeana"
#>   ..$ Family : chr "Meliaceae"
#>   ..$ 1998   : int 1
#>  $ cocoli_tree_basal_area.csv    :Classes 'tbl_df', 'tbl' and 'data.frame':	1 obs. of  3 variables:
#>   ..$ species: chr "Trichilia pleeana"
#>   ..$ Family : chr "Meliaceae"
#>   ..$ 1998   : num 0.00312
#>  $ sherman_sapling_abundance.csv :Classes 'tbl_df', 'tbl' and 'data.frame':	7 obs. of  5 variables:
#>   ..$ species: chr [1:7] "Bactris barronis" "Chrysophyllum argenteum" "Geonoma cuneata" "Guarea sherman" ...
#>   ..$ Family : chr [1:7] "Arecaceae" "Sapotaceae" "Arecaceae" "Meliaceae" ...
#>   ..$ 1996   : int [1:7] 0 1 1 0 0 0 0
#>   ..$ 1998   : int [1:7] 0 0 0 1 0 1 0
#>   ..$ 1999   : int [1:7] 1 0 0 0 1 0 1
#>  $ sherman_sapling_basal_area.csv:Classes 'tbl_df', 'tbl' and 'data.frame':	7 obs. of  5 variables:
#>   ..$ species: chr [1:7] "Bactris barronis" "Chrysophyllum argenteum" "Geonoma cuneata" "Guarea sherman" ...
#>   ..$ Family : chr [1:7] "Arecaceae" "Sapotaceae" "Arecaceae" "Meliaceae" ...
#>   ..$ 1996   : num [1:7] 0 0.000119 0.000119 0 0 ...
#>   ..$ 1998   : num [1:7] 0 0 0 0.000279 0 ...
#>   ..$ 1999   : num [1:7] 0.000135 0 0 0 0.00102 ...
#>  $ sherman_tree_abundance.csv    :Classes 'tbl_df', 'tbl' and 'data.frame':	1 obs. of  3 variables:
#>   ..$ species: chr "Perebea xanthochyma"
#>   ..$ Family : chr "Moraceae"
#>   ..$ 1999   : int 1
#>  $ sherman_tree_basal_area.csv   :Classes 'tbl_df', 'tbl' and 'data.frame':	1 obs. of  3 variables:
#>   ..$ species: chr "Perebea xanthochyma"
#>   ..$ Family : chr "Moraceae"
#>   ..$ 1999   : num 0.00308
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
#>    Latin             Family      Authority            `1994` `1997` `1998`
#>    <chr>             <chr>       <chr>                 <int>  <int>  <int>
#>  1 Acacia melanocer~ Fabaceae    Beurl.                   23     19     19
#>  2 Acalypha diversi~ Euphorbiac~ Jacq.                    14     18     22
#>  3 Acalypha macrost~ Euphorbiac~ Jacq.                     1      2      2
#>  4 Adelia triloba    Euphorbiac~ (Müll.Arg.) Hemsl.        5      6      6
#>  5 Aegiphila paname~ Lamiaceae   Moldenke                  2      1      1
#>  6 Albizia adinocep~ Fabaceae    (Donn. Sm.) Britton~     55     53     57
#>  7 Albizia procera   Fabaceae    (Roxb.) Benth.            2      2      2
#>  8 Alchornea costar~ Euphorbiac~ Pax & K. Hoffm. in ~      2      1      1
#>  9 Alibertia edulis  Rubiaceae   (Rich.) A. Rich.        253    228    224
#> 10 Alseis blackiana  Rubiaceae   Hemsl.                    4      4      4
#> # ... with 166 more rows
#> 
#> $shermanAbundTable.tsv
#> # A tibble: 270 x 7
#>    Latin             Family      Authority     `1996` `1997` `1999` `2009`
#>    <chr>             <chr>       <chr>          <int>  <int>  <int>  <int>
#>  1 Acacia sp.1       Fabaceae    <NA>               0      0      0      4
#>  2 Acacia melanocer~ Fabaceae    Beurl.             1      1      0      0
#>  3 Acalypha diversi~ Euphorbiac~ Jacq.              9      7      5     20
#>  4 Aegiphila paname~ Lamiaceae   Moldenke           1      0      0      0
#>  5 Alchornea sp.3    Euphorbiac~ <NA>              33     24     22     12
#>  6 Alchornea latifo~ Euphorbiac~ Sw.               93     90     94     96
#>  7 Alibertia edulis  Rubiaceae   (Rich.) A. R~      2      2      2      0
#>  8 Amaioua corymbosa Rubiaceae   Kunth             89     88     89     83
#>  9 Andira inermis    Fabaceae    (W. Wright) ~     41     38     38     37
#> 10 Annona spraguei   Annonaceae  Saff.             15      9      7     12
#> # ... with 260 more rows
```

