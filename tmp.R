tiny <- tibble::tribble(
  ~Tag, ~PlotName, ~Status, ~DBH,   ~ExactDate, ~PlotCensusNumber, ~CensusID, ~Genus, ~SpeciesName, ~Family,
  "0001",     "p", "alive",   1L, "2000-01-01",                1L,        1L,    "A",          "a",     "f",
  "0001",     "p",  "dead",   1L, "2001-01-01",                2L,        2L,    "A",          "a",     "f",
  "0002",     "p", "alive",  10L, "2000-01-01",                1L,        1L,    "B",          "b",     "f",
  "0002",     "p", "alive",  10L, "2001-01-01",                2L,        2L,    "B",          "b",     "f",
)
fgeo_abundance(tiny)
suppressMessages(fgeo_basal_area(tiny))
basal_area(1)










test_that("known output", {
  alive <- suppressWarnings(suppressMessages(fgeo_basal_area(tiny)))
  expect_equal(alive$`2000`, c(basal_area(1), basal_area(1)))
  expect_equal(alive$`2001`, c(basal_area(1), basal_area(1)))
  
  
  expect_equal(one_dead_in_cns2$`2000`, c(basal_area(1), basal_area(1)))
  expect_equal(one_dead_in_cns2$`2001`, c(0, basal_area(1)))
  
  tiny_22d <- tiny
  tiny_22d[tiny_22d$Tag == "0002" & tiny_22d$CensusID == 2, ]$Status <- "dead"
  two_dead_in_cns2 <- suppressWarnings(suppressMessages(fgeo_basal_area(tiny_22d)))
  expect_equal(two_dead_in_cns2$`2000`, c(basal_area(1), basal_area(1)))
  expect_equal(two_dead_in_cns2$`2001`, c(basal_area(1), 0))
})







# Stuff











fgeo_ba <- function(vft, plot_nm) {
  pick_plotname(vft, plot_nm) %>% 
    fgeo.base::pick_dbh_min(1) %>% 
    filter_tree_status_by_census(.status = "alive", exclude = FALSE) %>%
    mean_years() %>% 
    fgeo_basal_area()
}
fgeo_abund <- function(vft, plot_nm) {
  pick_plotname(vft, plot_nm) %>% 
    fgeo.base::pick_dbh_min(1) %>% 
    filter_tree_status_by_census(.status = "alive", exclude = FALSE) %>%
    mean_years() %>% 
    fgeo_abundance()
}

plots <- sort(unique(vft$PlotName))

# FIXME:  
# Error in sanitize_Status_DBH_ExaxtDate(vft, valid_status) : 
# argument "valid_status" is missing, with no default
purrr::map(plots, ~fgeo_ba(vft, .x))
purrr::map(plots, ~fgeo_abund(vft, .x))
