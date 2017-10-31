context("map_sp")

# Minimal data to show
census <- bci::bci12full7
# Filtering only 3 species for a minimal example.
(species_selected <- unique(census$sp)[1:3])
census_sub <- dplyr::filter(census, sp %in% species_selected)
# Sampling only 1000 rows for a quick example
census_sub <- dplyr::sample_n(census_sub, 1000)

# Selecting all species in the example dataset
all_species <- unique(census_sub$sp)



test_that("errs with wrong inputs.", {
  # wrong species
  expect_error(map_sp(species = 1:3, census = census_sub))
  expect_error(map_sp(
    species = all_species,
    # wrong name
    census = dplyr::rename(census_sub, spp = sp)
    )
  )
})

test_that("outputs a non empty list", {
  result <- map_sp(species = all_species, census = census_sub)
  expect_type(result, "list")
  expect_silent(
    lapply(result, assertive::is_non_empty) %>%
      unlist() %>%
      all()
  )
})

test_that("output is a named list with names equal to species codes in sp", {
  result <- map_sp(species = all_species, census = census_sub)
  expect_named(result, all_species)
})

test_that("output is an invisible and named list and prints pdf w/ message", {
  expect_message(
    result <- map_sp_pdf(species = all_species, census = census_sub)
  )
  expect_type(result, "list")
  expect_length(result, length(all_species))
  expect_named(result, all_species)
})

