## ----setup, echo = FALSE, message=FALSE, warning=FALSE-------------------
# hadley's settings
set.seed(1014)
options(digits = 3)

knitr::opts_chunk$set(
  echo = TRUE,  # {mine}
  comment = "#>",
  collapse = TRUE,
  # cache = TRUE,
  out.width = "70%",
  fig.align = "center",
  fig.width = 6,
  fig.asp = 0.618,  # 1 / phi
  fig.show = "hold"
)

options(dplyr.print_min = 6, dplyr.print_max = 6)

## ----pkgs----------------------------------------------------------------
library(tidyverse)
library(forestr)

## ----stem----------------------------------------------------------------
# tibble has an informative print method
stem0 <- as_tibble(bci12s7mini)
stem0

## ------------------------------------------------------------------------
# getting biomass as it comes out of the box from the analytical tables
stem0$biomass <- stem0$agb

# focusing only on a few variables we care about
stem1 <- select(stem0, 
  treeID, stemID, quadrat, status, sp, biomass, 
)
stem1

## ------------------------------------------------------------------------
add_id <- function(x) {
  x$id <- paste0(x$treeID, "_", x$stemID)
  x
}
stem2 <- add_id(stem1)
stem2

## ------------------------------------------------------------------------
# Removing redundant variables; also placing id first, and biomass last
stem3 <- select(stem2, id, quadrat, status, sp, biomass)
stem3

## ------------------------------------------------------------------------
by_sp <- group_by(stem3, sp)
by_sp <- dplyr::summarize(by_sp, biomass = sum(biomass, na.rm = TRUE))

## ------------------------------------------------------------------------
by_sp_quad <- group_by(stem3, sp, quadrat)
by_sp_quad <- summarize(by_sp_quad, biomass = sum(biomass, na.rm = TRUE))

## ------------------------------------------------------------------------
filter(by_sp_quad, biomass > 0)

