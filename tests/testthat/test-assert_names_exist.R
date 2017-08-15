context("assert_names_exist")

library(dplyr)
# Filter species with abundant alive stems
alive_n_plus <- sp_alive_n(bci::bci12full7, 5000)

# Randomly select a relatively high number of stems
bci_mini <- bci::bci12full7 %>%
  dplyr::filter(sp %in% alive_n_plus) %>%
  dplyr::sample_n(10000)

test_that("data without required names throws an error", {
  # change names
  names(bci_mini) <- letters[seq_along(names(bci_mini))]
  expect_error(assert_names_tag_sp_exist(bci_mini))

  # change names
  names(bci_habitat) <- letters[seq_along(names(bci_habitat))]
  expect_error(assert_names_x_y_exist(bci_habitat))
})
