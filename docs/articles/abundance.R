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

options(dplyr.print_min = 10, dplyr.print_max = 10)

## ------------------------------------------------------------------------
# install.packages("remotes")
remotes::install_github(repo = "forestgeo/fgeo.abundance")
library(fgeo.abundance)

## ------------------------------------------------------------------------
stem <- bciex::bci12s7mini

n <- abundance(stem)
head(n)

# Of course you can change the defaults
head(abundance(stem, "sp", only_alive = FALSE))

## ---- error=TRUE---------------------------------------------------------
abundance_tally(stem)
abundance_tally(stem, only_alive = FALSE)

# This won't work
fails <- abundance(stem, group_by = NULL)

## ---- error=TRUE---------------------------------------------------------
# With fgeo.abundance you can assign a string of variable names and pass use it later
quoted_vars <- c("quadrat", "sp")
with_fgeo.abundance <- abundance(stem, quoted_vars)

# Compare
library(dplyr)
alive <- dplyr::filter(stem, status == "A")
# With dplyr you use bare variable names
with_dplyr <- dplyr::count(alive, quadrat, sp)

# You cant reuse grouping variables saved as strings
# (other tricks work but are beyond the scope of this tutorial)
fails <- dplyr::count(alive, quoted_vars)
# This also fails, not but trowing an error but by giving the wrong output
also_fails <- dplyr::count(alive, "quadrat", "sp")

# Whatever you choose, if you do it right you get the same results
alternatives <- list(with_fgeo.abundance, with_dplyr)
lapply(alternatives, head)

is_var_identical <- function(x, y, var) {identical(x[["var"]], y[["var"]])}
is_var_identical(with_fgeo.abundance, with_dplyr, "quadrat")
is_var_identical(with_fgeo.abundance, with_dplyr, "sp")
is_var_identical(with_fgeo.abundance, with_dplyr, "n")

## ------------------------------------------------------------------------
stem <- bciex::bci12s7mini
ba <- basal_area(stem)
head(ba)

# Same but silent
silent <- suppressMessages(basal_area(stem))
head(silent)

## ------------------------------------------------------------------------
ba <- basal_area_ind(stem$dbh)
head(ba)

# Why NAs?
head(stem$dbh)
# because `dbh` is na in the data set

