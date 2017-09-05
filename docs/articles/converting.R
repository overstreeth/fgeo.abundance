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

stem <- bci12s6mini

## ------------------------------------------------------------------------
# USING `abundance_n()`

x1 <- abundance_n(stem, group_by = c("sp"), only_alive = FALSE)
length(dim(x1))

head(x1)
class(x1)

head(as.data.frame(x1))
# same
head(as.data.frame.array(x1))
# a little better
head(as.data.frame.table(x1))

# The previous output is the closest to this "tidy" data frame:
head(n_dplyr_se(stem, "sp", only_alive = FALSE))



# SAME, USING `abundance_n2()`

x1 <- abundance_n(stem, group_by = c("sp"), only_alive = FALSE)

head(as.data.frame.table(x1))

head(n_dplyr_se(stem, "sp", only_alive = FALSE))

## ------------------------------------------------------------------------
# USING `abundance_n()`

x2 <- abundance_n(stem, group_by = c("sp", "status"), only_alive = FALSE)
length(dim(x2))

head(x2)
class(x2)

head(as.data.frame(x2))
# same
head(as.data.frame.array(x2))
# a little better
head(arrange(as.data.frame.table(x2), sp))

# The previous output is the closest to this "tidy" data frame:
head(n_dplyr_se(stem, c("sp", "status"), only_alive = FALSE))



# SAME, USING `abundance_n2()`

x1 <- abundance_n(stem, group_by = c("sp"), only_alive = FALSE)

head(as.data.frame.table(x1))

head(n_dplyr_se(stem, "sp", only_alive = FALSE))

## ------------------------------------------------------------------------
df <- n_dplyr_se(stem, groups = c("sp", "status"), only_alive = FALSE)
head(df)
a <- daply(df, .(sp, status), nrow)
head(a)
class(a)

## ------------------------------------------------------------------------
example_years <- c(1938, 1956, 1909, 1992)
example <- dplyr::filter(baseball, year %in% example_years)

# grouping by year, team and stint, to show array of 3 dimensions
groups <- c("year", "team", "stint")
(a <- daply(example, .variables = groups, .fun = nrow))
is.array(a)

