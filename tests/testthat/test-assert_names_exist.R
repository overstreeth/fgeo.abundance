context("assert_names_exist")

test_that("data without required names throws an error", {
  bci_mini <- bci_mini
  # change names
  names(bci_mini) <- letters[seq_along(names(bci_mini))]
  expect_error(assert_names_tag_sp_exist(bci_mini))

  # change names
  names(bci_habitat) <- letters[seq_along(names(bci_habitat))]
  expect_error(assert_names_x_y_exist(bci_habitat))
})
