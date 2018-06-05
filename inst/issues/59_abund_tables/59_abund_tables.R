# From Suzanne:
# In the datasets, you should look at the "plotcensusnumber" or "censusid"
# instead of the years. Some censuses may take more than one year, or there may
# be some big trees that do not get measured until the next year, during the
# summer. The years in the publications refer to the average or the median year.

# Cocoli should have the exact same number of censuses, but Sherman will have
# one more census. Please download the datasets (BCI and Sherman/Cocoli again)
# from the following site:



library(tidyverse)
library(fs)

# Data from Suzanne -------------------------------------------------------

path <- "../fgeo.data/inst/private/data-raw-private"
vft_bci <- fgeo.tool::read_vft(fs::path(path, "ViewFullTable_bci.csv"))
vft_sher_coco <- fgeo.tool::read_vft(
  fs::path(path, "ViewFullTable_sherman_cocoli.csv")
)

# TODO: Make tables.




# Publication -------------------------------------------------------------

# Source: https://dash.ucdavis.edu/stash/dataset/doi:10.15146/R3MM4V

fs::dir_ls("inst/issues/59_abund_tables/condit_et_al") %>% 
  map(readr::r)

path <- "inst/issues/59_abund_tables/condit_et_al"
path %>% 
  dir_ls() %>% 
  map(readr::read_delim, "\t") %>% 
  set_names(path_file(dir_ls(path))) %>% 
  map(select, -Latin, -Family, - Authority) %>% 
  map(names)
