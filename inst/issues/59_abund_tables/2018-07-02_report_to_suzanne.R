# Suzanne -----------------------------------------------------------------

# Report: /bookdown.org/fgeocomm/create_table/

# > To get the abundance, first you need to extract one record for each tree (i.e.
# one treeid for each census and plot). You have to extract the record with the
# highest dbh for each treeid per census per plot.
# --- Suzanne

# Hi Suzanne, 
# I may be missing something but I think the issue of counting counting
# unique trees is simpler. I believe I only need to count distinct `TreeID` by
# `PlotName` by `PlotCensusNumber`*** by `species` (`Genus` + `SpeciesName`).
# This I can do  it regardless of `DBH`.
#
# *** This is a simplification. What I really need is not PlotCensusNumber but
# round mean year.

# Here is an example:
library(tidyverse)
library(fgeo)

# There are two trees of the same species in census 1 and one tree in census
# 2, each tree has two stems.
vft_toy <- tribble(
  ~PlotName, ~PlotCensusNumber, ~TreeID, ~StemID, ~Genus, ~SpeciesName,
  "luq",                 1,     "1",   "1.1",    "A",          "a",
  "luq",                 1,     "1",   "1.2",    "A",          "a",
  # -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
  "luq",                 1,     "2",   "2.1",    "A",          "a",
  "luq",                 1,     "2",   "2.2",    "A",          "a",
  # == == ==  == == ==  == == ==  == == ==  == == ==  == == ==  ==
  "luq",                 2,     "1",   "1.2",    "A",          "a",
  "luq",                 2,     "1",   "1.1",    "A",          "a"
)
vft_toy

# Regardless of how many stems each tree has, the count of distinct `TreeID` in
# each group is this:
# * 2 in census 1
# * 1 in census 2 
vft_toy %>% 
  unite("species", Genus, SpeciesName) %>% 
  group_by(PlotName, PlotCensusNumber, species) %>% 
  summarize(n = n_distinct(TreeID))

# The complete solution is incorporated in `abundance_byyr()`.



# > In general, the stem tables are used to calculate biomass and basal areas,
# where we use the diameters of ALL the stems to get a better estimate.

# OK. This is incorporated in `basal_area_byyr()`.
