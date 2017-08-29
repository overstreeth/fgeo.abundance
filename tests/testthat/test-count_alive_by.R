context("id_alive")

test_that("has the extra variable id", {
  x <- bciex::bci12t7mini
  expect_equal(length(id_alive(x)), length(x) + 1)
  
  expect_true(
    any(grepl(
      "id", names(id_alive(x))
    ))
  )
})



context("count_alive")

test_that("outputs the same as ctfs::abundance when type = 'abund'.", {
  # Tree data
  now <- count_alive(bciex::bci12t1mini)
  before <- unname(ctfs::abundance(bciex::bci12t1mini)$abund$all)
  expect_equal(before, now)
  # Stem data
  now <- count_alive(bciex::bci12s1mini)
  before <- unname(ctfs::abundance(bciex::bci12s1mini)$abund$all)
  expect_equal(before, now)
})



context("count_alive_by")

test_that("outputs the same as ctfs::abundance with defaults", {
  x <- bciex::bci12t7mini
  now <- count_alive_by(x, sp) %>% arrange(sp)
  before <- x %>%
    split(.$sp) %>%
    purrr::map(ctfs::abundance) %>%
    purrr::map("abund") %>%
    tibble::enframe() %>%
    tidyr::unnest() %>% 
    rename(sp = name, n = all) %>% 
    arrange(sp)
  expect_equal(now, before)
})
