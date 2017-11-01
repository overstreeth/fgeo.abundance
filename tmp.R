library(tidyverse)
census <- bciex::bci12t7mini

few_sp <- count(census, sp) %>% arrange(n) %>% tail(3) %>% pull(sp)
census <- census %>% filter(sp  %in% few_sp)

elevation <- bciex::bci_elevation
elevation <- dplyr::rename(elevation, gx = x, gy = y)

under250 <- census %>% filter(gx <250, gy < 250) 
map_sp(under250, "hybapr", xlim = c(0, 800), ylim = c(0, 900))
map_sp(census, "hybapr")

xlimit <- c(0, max(census$gx, na.rm = TRUE))
ylimit <- c(0, max(census$gy, na.rm = TRUE))




p <- map_basic(census, xlimit, ylimit)

myelev <- elevation
add_elevation(p, myelev, line_size = .75)

map_one_sp(census = census, one_sp = "hybapr")


map_one_sp(census, "hybapr", elevation = elevation, low = "red", high = "black",
  theme = ggplot2::theme(text = element_text(size = 20)), line_size = .2)


map_sp(census, c("hybapr", "faraoc"), theme = ggplot2::theme_dark(),
  elevation = elevation)

check_map_sp(census, "hybapr")


species <- c("hybapr", "faraoc")
map_sp_pdf(census, species, file = "mymap.pdf", theme = ggplot2::theme_minimal(),
  elevation = elevation, bins = 2, line_size = 2)


