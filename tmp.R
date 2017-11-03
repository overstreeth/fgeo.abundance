library(tidyverse)



census <- forestr::bci12t7mini

few_sp <- count(census, sp) %>% arrange(n) %>% tail(3) %>% pull(sp)
census <- census %>% filter(sp  %in% few_sp)

map_one_sp2(census, unique(census$sp)[1], NULL, NULL) 





theme <- theme_dark()

d <- data.frame(x = 1:10, y = 1:10)
ggplot(d, aes(x, y)) +
  geom_point() +
  scale_x_continuous(
    breaks = 1:10, 
    minor_breaks = 

    ) +
  theme +
  theme(panel.grid.major = element_blank())






print(map_sp_invisible(census, "hybapr"))
map_sp_pdf(census, "hybapr")



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


