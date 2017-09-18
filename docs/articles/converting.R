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
library(plyr)
library(dplyr)

stem <- bci12s7mini

## ------------------------------------------------------------------------
# USING `abundance_n()`

abundance_n <- function(x, group_by = c("quadrat", "sp"), only_alive = TRUE) {
    if (only_alive) {
      valid.status <- "A"
    } else {
      valid.status <- unique(x$status)
    }
    table(x[x$status %in% valid.status, group_by])
}

x1 <- abundance_n(stem, group_by = c("sp"), only_alive = FALSE)
length(dim(x1))

head(x1)
class(x1)

head(as.data.frame(x1))
# same
head(as.data.frame.array(x1))
# a little better
head(as.data.frame.table(x1))

## ------------------------------------------------------------------------
df <- abundance(stem, group_by = c("sp", "status"), only_alive = FALSE)
head(df)
a <- plyr::daply(df, .(sp, status), nrow)
head(a)
is.array(a)

## ------------------------------------------------------------------------
example_years <- c(1938, 1956, 1909, 1992)
example <- dplyr::filter(baseball, year %in% example_years)

# grouping by year, team and stint, to show array of 3 dimensions
groups <- c("year", "team", "stint")
(a <- plyr::daply(example, .variables = groups, .fun = nrow))
is.array(a)

