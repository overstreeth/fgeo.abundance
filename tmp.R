library(tidyverse)
library(fgeo.abundance)
library(vegan)
data(BCI)

bci_wide <- BCI %>% 
  rowid_to_column() %>% 
  as.tibble()
bci_long <- gather(bci_wide, "sp", "n", -rowid)

identical_diversity <- function(index, .f) {
 df_test_diversity(index, .f) %>% 
    pull() %>% 
    all()
}

df_test_diversity <- function(index, .f) {
  diversity_wide <- .f(bci_wide[-1], index)
  diversity_long <- vgn_diversity(group_by(bci_long, rowid), n, index)
  tibble(
    long = round(diversity_long$value, 1), 
    wide = round(diversity_wide, 1)
  ) %>% 
    mutate(is_equal = identical(long, wide))
}

# vgn_diversity() outputs equal to vegan::diversity()
c("shannon", "simpson", "invsimpson") %>% 
  lapply(identical_diversity, vegan::diversity) %>% 
  unlist() %>% 
  all()

# vgn_diversity() outputs equal to vegan::specnumber()
diversity_wide <- vegan::specnumber(bci_wide[-1])
diversity_long <- vgn_diversity(group_by(bci_long, rowid), n, "specnumber")
tibble(
  long = round(diversity_long$value, 1), 
  wide = round(diversity_wide, 1)
) %>% 
  mutate(is_equal = identical(long, wide)) %>% 
  pull() %>% 
  all() %>% 
  expect_true()



# Ceros don't matter
with_cero <- c(1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0)
no_cero <- c(1, 1, 1, 1, 1)

# Ceros don't matter for vegan::diversity()
.indices <- c("shannon", "simpson", "invsimpson")
identical(
  .indices %>% map_dbl(~vegan::diversity(with_cero, .x)), 
  .indices %>% map_dbl(~vegan::diversity(no_cero, .x))
) %>% 
  expect_true()


# Ceros don't matter for specnumber()
list(with_cero, no_cero) %>% 
  map_dbl(vegan::specnumber) %>% 
  unique() %>% 
  length() == 1 %>% 
  expect_true()
  
