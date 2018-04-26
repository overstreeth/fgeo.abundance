library(tidyverse)
library(fgeo.data)
library(janitor)

pdim <- c(320, 500)
idx <- 1:nrow(fgeo.data::luquillo_habitat)
cns <- fgeo.data::luquillo_tree6_random %>% 
  filter(!is.na(sp), dbh >= 10)
gsz <- 20



# Building blocks to produce output analog to abundanceperquad().

# How to generalize it to fit vegan requirements?
# count_index(by = sp)
var <- quo(sp)
count_index <- cns %>% 
  fgeo.tool::add_index(plotdim = pdim) %>%
  right_join(tibble(index = idx)) %>% 
  count(index, !! var)

# Useful for vegan objects
# spread_n(key = sp, by = index, fill = 0)
spread_n <- count_index %>% 
  tidyr::spread(sp, n, fill = 0) %>%
  arrange(index) %>% 
  select(-index, -`<NA>`)

# transpose_df()
transpose_df <- spread_n %>% 
  t() %>% 
  as.data.frame() %>% 
  rlang::set_names(rownames(spread_n))

transpose_tbl <- add_column(sp = rownames(.), .before = 1) %>% 
  as.tibble()





# faster than ctfs
tbl <-  cns %>% 
  fgeo.tool::add_index(plotdim = c(320, 500)) %>% 
  janitor::tabyl(sp, index)
rownames(tbl) <- tbl$sp
tbl <- tbl %>% 
  select(-sp) %>% 
  ctfs::fill.dimension(rownames(.), idx)
as_tibble(tbl)

# Super slow because it does a lot more work than needed
ctfs <- ctfs::abundanceperquad(
  cns, mindbh = 10, plotdim = pdim, gridsize = gsz
)$abund
as_tibble(ctfs)

# Test
identical(mine, tbl)

# Something seems wrong with ctfs()
identical(mine, ctfs)
identical(tbl, ctfs)

one_sp <- unique(cns$sp)[[1]]
tt_test_one(one_sp, luquillo_habitat, mine, pdim, gsz)
