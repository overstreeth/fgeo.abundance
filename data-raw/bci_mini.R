# Source: see https://github.com/forestgeo/bci

library(dplyr)

# Create a small dataset for examples

# Filter species with abundant alive stems
alive_n_plus <- sp_alive_n(bci::bci12full7, 5000)

# Randomly select a relatively high number of stems
bci_mini <- bci::bci12full7 %>%
  filter(sp %in% alive_n_plus) %>%
  sample_n(10000)

use_data(bci_mini, overwrite = TRUE)

