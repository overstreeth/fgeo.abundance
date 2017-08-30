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

## ------------------------------------------------------------------------
library(forestr)
library(bci)
library(dplyr)

x <- bci12full7

system.time({
  with_dplyr <- count_status(x = x, .status = "A", sp)
  with_dplyr <- arrange(with_dplyr, sp)
})

system.time({
  # There may be a faster and more elegant way to do this in base
  .split <- split(x, x$sp)
  applied_1 <- lapply(.split, ctfs::abundance)
  applied_2 <- lapply(applied_1, "[[", "abund")
  
  sp <- names(applied_2)
  n <- lapply(applied_2, "[[", "all")
  n <- unlist(n)
  with_base <- data.frame(sp, n, stringsAsFactors = FALSE)
  with_base <- with_base[order(with_base$sp), ]
})

head(with_dplyr)

head(with_base)

all.equal(with_dplyr, with_base)

