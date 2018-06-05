library(tidyverse)
library(fs)

# Cocoli and Sherman ------------------------------------------------------

# Source of ViewFullTable.csv: Suzanne Lao via http://bit.ly/2sJbDQp.

file <- here::here("inst/issues/59_abund_tables/ViewFullTable.csv")
vft <- fgeo.tool::read_vft(file)
cocoli <- vft %>% filter(PlotName == "cocoli")
sherman <- vft %>% filter(PlotName == "sherman")

vft %>%
  select(PlotName, ExactDate) %>% 
  group_by(PlotName) %>% 
  transmute(years = gsub("(^....).*", "\\1", ExactDate)) %>% 
  unique() %>% 
  arrange(PlotName, years)



# BCI ---------------------------------------------------------------------

library(tidyverse)

dates <- "../bci/data-raw/ViewFullTable/ViewFullTable.txt" %>% 
  fgeo.tool::read_vft() %>% 
  pull(ExactDate)

years <- year(dates) %>% unique()
tibble(BCI = years) %>% 
  arrange(years) %>% 
  print(n = nrow(.))



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
