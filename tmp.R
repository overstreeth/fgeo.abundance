vft <- tibble::tribble(
  ~Tag, ~PlotName,   ~Status, ~DBH,   ~ExactDate, ~PlotCensusNumber, ~CensusID, ~Genus, ~SpeciesName, ~Family,
  "0001",     "p",   "alive",   1L, "2000-01-01",                1L,        1L,    "A",          "a",     "f",
  "0001",     "p",    "dead",   1L, "2001-01-01",                2L,        2L,    "A",          "a",     "f",
  "0002",     "p",   "alive",  10L, "2000-01-01",                1L,        1L,    "B",          "b",     "f",
  "0002",     "p", "missing",  10L, "2001-01-01",                2L,        2L,    "B",          "b",     "f",
)
fgeo_abundance(vft)
suppressWarnings(fgeo_basal_area(vft))
basal_area(1)
basal_area(10)

# You may want to filter the data first
pick <- vft %>% 
  pick_plotname("p") %>% 
  fgeo.base::pick_dbh_min(10)
