# FIXME: NA columns with cocoli and sherman
# FIXME: Error when map()ing over censuses

library(fgeo.abundance)
vft <- readr::read_csv(here::here("inst/issues/59_abund_tables/vft_pick.csv"))

pick_one_plot(vft, "bci") %>%
pick_one_plot(vft, "bci", .status = "alive", exclude = FALSE) %>%
  mean_years() %>% 
  fgeo_abundance()

pick_one_plot(vft, "bci") %>%
  mean_years() %>% 
  fgeo_basal_area()

# NA year!!!
pick_one_plot(vft, "cocoli") %>%
  mean_years() %>% 
  fgeo_basal_area()

# NA year!!!
pick_one_plot(vft, "cocoli") %>%
  mean_years() %>% 
  fgeo_abundance()

# NA year!!!
pick_one_plot(vft, "sherman") %>%
  mean_years() %>% 
  fgeo_abundance()

# NA year!!!
pick_one_plot(vft, "sherman") %>%
  mean_years() %>% 
  fgeo_basal_area()



fgeo_abund <- function(vft, plot_nm) {
  pick_one_plot(vft, plot_nm) %>%
    mean_years() %>% 
    fgeo_abundance()
}

fgeo_ba <- function(vft, plot_nm) {
  pick_one_plot(vft, plot_nm) %>%
    mean_years() %>% 
    fgeo_basal_area()
}

plots <- sort(unique(vft$PlotName))
purrr::map(plots, ~fgeo_abund(vft, .x))
purrr::map(plots, ~fgeo_ba(vft, .x))





