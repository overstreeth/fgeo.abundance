library(tidyverse)
cns_one_sp <- dplyr::sample_n(census[census$sp == "hybapr", ], 200)
elevation <- bciex::bci_elevation
elevation <- dplyr::rename(elevation, gx = x, gy = y)
xlim <- c(0, max(cns_one_sp$gx, na.rm = TRUE))
ylim <- c(0, max(cns_one_sp$gy, na.rm = TRUE))


mymap <- function(census, xlim, ylim, ...) {
  check_crucial_names(census, c("gx", "gy"))
  p <- ggplot2::ggplot(
    data = census,
    ggplot2::aes(x = gx, y = gy)
  ) +
    ggplot2::geom_point(...) +
    ggplot2::facet_grid(. ~ sp) +
    ggplot2::coord_fixed(xlim = xlim, ylim = ylim) +
    ggplot2::labs(x = NULL, y = NULL) +
    ggplot2::theme_bw()
}




p <- map_xy(cns_one_sp, xlim, ylim)

add_elevation(p, elevation = elevation)


