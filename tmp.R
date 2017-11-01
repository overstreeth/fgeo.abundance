library(tidyverse)
census <- bciex::bci12t7mini

few_sp <- count(census, sp) %>% arrange(n) %>% tail(3) %>% pull(sp)
census <- census %>% filter(sp  %in% few_sp)

elevation <- bciex::bci_elevation
elevation <- dplyr::rename(elevation, gx = x, gy = y)
xlimit <- c(0, max(census$gx, na.rm = TRUE))
ylimit <- c(0, max(census$gy, na.rm = TRUE))

map_xy(census, xlimit, ylimit)
map_one_sp(census, "hybapr", elevation = elevation, bins = NULL)





map_sp(census, c("hybapr", "faraoc"), elevation = elevation, bins = 20)

species <- c("hybapr", "faraoc")
map_sp_pdf(census, species, file = "mymap.pdf", elevation = elevation, bins = 2)


