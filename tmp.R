load_all()
vft <- readr::read_csv(here::here("inst/issues/59_abund_tables/vft_pick.csv"))

pick_plotname(vft, "bci") %>% 
  fgeo.base::pick_dbh_min(1) %>% 
  # TODO: Test
  filter_tree_status(.status = "alive", exclude = FALSE) %>%
  mean_years() %>% 
  fgeo_abundance()

fgeo_ba <- function(vft, plot_nm) {
  pick_plotname(vft, plot_nm) %>% 
    fgeo.base::pick_dbh_min(1) %>% 
    filter_tree_status(.status = "alive", exclude = FALSE) %>%
    mean_years() %>% 
    fgeo_basal_area()
}
fgeo_abund <- function(vft, plot_nm) {
  pick_plotname(vft, plot_nm) %>% 
    fgeo.base::pick_dbh_min(1) %>% 
    filter_tree_status(.status = "alive", exclude = FALSE) %>%
    mean_years() %>% 
    fgeo_abundance()
}

plots <- sort(unique(vft$PlotName))
purrr::map(plots, ~fgeo_ba(vft, .x))
purrr::map(plots, ~fgeo_abund(vft, .x))
