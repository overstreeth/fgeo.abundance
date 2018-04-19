library(vegan)
library(fgeo.abundance)
library(tidyverse)

census <- data.frame(
  quadrat = rep(c("0000", "0001"), each = 3),
  sp = rep(paste0("sp", 1:3), 2),
  n = sample.int(100, 6),
  stringsAsFactors = FALSE
)
census

vgn_diversity(census, n)
vgn_diversity(census, n, index = "shannon")
vgn_diversity(census, n, index = "simpson")
vgn_diversity(census, n, index = c("simpson", "shannon"))
vgn_diversity(census, n, index = c("specnumber", "simpson", "shannon"))
vgn_diversity(census, n, index = c("simpson", "shannon", "specnumber"))
vgn_diversity(census, n, index = "shannon")
vgn_diversity(census, n, index = "specnumber")
