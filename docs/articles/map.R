## ----setup, echo = FALSE, message=FALSE, warning=FALSE-------------------
# hadley's settings
set.seed(1014)
options(digits = 3)

knitr::opts_chunk$set(
  echo = TRUE,
  comment = "#>",
  collapse = TRUE,
  cache = TRUE,
  out.width = "70%",
  fig.align = "center",
  fig.width = 6,
  fig.asp = 0.618,  # 1 / phi
  fig.show = "hold"
)

library(ggplot2)
update_geom_defaults("text", list(size = 14.5))

## ------------------------------------------------------------------------
GITHUB_PAT <- "your token"
# install_github("forestgeo/forestr", auth_token = GITHUB_PAT)
library(forestr)

# Also using the package dplyr for easier data manipulation;
# it is the most downloaded R package (https://goo.gl/bXBmkS).
library(dplyr)
# Print only a few rows of tibbles (modern dataframes) to save space
options(dplyr.print_min = 6, dplyr.print_max = 6)

## ------------------------------------------------------------------------
example_data <- dplyr::as_tibble(forestr::bci12t7mini)

# Let's find the top-3 most abundant species
sp_n <- count(example_data, sp)
arranged <- arrange(sp_n, desc(n))
sp_top3 <- head(arranged, 3)$sp
sp_top3

# For a this example, let's filter the data set to keep only the top-3 species
census <- filter(example_data, sp  %in% sp_top3)
census

## ------------------------------------------------------------------------
# Print to screen
map_sp(census, c("hybapr", "faraoc"))

# Save in a .pdf (defaults to working directory)
map_sp_pdf(census, c("hybapr", "faraoc"))

## ---- error=TRUE---------------------------------------------------------
# Show behaviour with wrong name
with_wrong_name <- rename(census, SP = sp)

# This will fail because the data set has name `SP`, not `sp`
map_sp(with_wrong_name, c("hybapr", "faraoc"))

## ------------------------------------------------------------------------
my_elev <- bciex::bci_elevation
my_elev

## ---- error=TRUE---------------------------------------------------------
# This will fail because the names of `my_elev` are not correct
map_sp(census, c("hybapr", "faraoc"), elevation = my_elev)

## ------------------------------------------------------------------------
# Renaming data and trying again
elev <- rename(my_elev, gx = x, gy = y)
map_sp(census, c("hybapr", "faraoc"), elevation = elev)

## ------------------------------------------------------------------------
map_sp(census, "hybapr",
  # Arguments passed to ggplot2::geom_point()
  size = 4, shape = 22, fill = "green", colour = "black", stroke = 2)

## ------------------------------------------------------------------------
map_sp(census, "hybapr", theme = ggplot2::theme_classic())

## ------------------------------------------------------------------------
map_sp(census, "hybapr", 
  elevation = elev, line_size = 1, low = "red", high = "blue", bins = 2)

## ---- error=TRUE---------------------------------------------------------
spp <- c("hybapr", "faraoc")
map_sp_pdf(census, spp, elevation = elev, file = "bad-name1")
map_sp_pdf(census, spp, elevation = elev, file = "bad-name2.png")
map_sp_pdf(census, spp, elevation = elev, file = "good-name.pdf")

## ------------------------------------------------------------------------
# Fake case: The plot maximum-limits are gx = 1000 and gy = 500; but all stems
# in the census occurr at lower gx and gy limits
rare_case <- filter(census, gx < 800, gy < 200)
map_sp(rare_case, unique(rare_case$sp))

## ------------------------------------------------------------------------
map_sp(rare_case, unique(rare_case$sp), xlim = c(0, 1000), ylim = c(0, 500))

## ------------------------------------------------------------------------
library(gridExtra)
maps <- map_sp(census, sp_top3)
multipaged <- marrangeGrob(map_sp(census, sp_top3), nrow = 1, ncol = 2)
multipaged

## ------------------------------------------------------------------------
# Saving to .pdf: Option 1
ggplot2::ggsave("multi-paged.pdf", multipaged)

# Saving to .pdf: Option 2
pdf()
multipaged
dev.off()

## ------------------------------------------------------------------------
# Saving to .png; this will create multiple files.
png()
multipaged
dev.off()

## ------------------------------------------------------------------------
# Showing only a few options; see all the options with ?ggplot2::theme()
mytheme <- ggplot2::theme(
  panel.background = element_rect(fill = "lightgreen"),
  panel.grid.minor = element_line(linetype = "dashed"),
  strip.background = element_rect(fill = "black"),
  strip.text = element_text(colour = "white"),
  text = element_text(size = 25, face = "bold")
)
map_sp(census, "hybapr", theme = mytheme)

