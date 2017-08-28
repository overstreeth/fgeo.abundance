# Filters alive and adds unique id
id_alive <- function(x) {
  alive <- x[x$status == "A", ]
  alive$id <- paste0(alive$treeID, "_", alive$stemID)
  alive
}

context("id_alive")
test_that("has the extra variable id", {
  x <- bci12t7mini
  expect_equal(length(id_alive(x)), length(x) + 1)
  
  expect_true(
    any(grepl(
      "id", names(id_alive(x))
    ))
  )
})



count_alive <- function(x) {
  alive_id <- id_alive(x)
  length(unique(alive_id$id))
}

library(testthat)
context("count_alive")

test_that("outputs the same as ctfs::abundance when type = 'abund'.", {
  # Tree data
  now <- count_alive(bci12t1mini)
  before <- unname(ctfs::abundance(bci12t1mini)$abund$all)
  expect_equal(before, now)
  # Stem data
  now <- count_alive(bci12s1mini)
  before <- unname(ctfs::abundance(bci12s1mini)$abund$all)
  expect_equal(before, now)
})



count_alive_by <- function(x, ..., wt = NULL, sort = FALSE)) {
  alive_id <- id_alive(x)
  dplyr::count(alive_id, ..., wt = wt, sort = sort))
}

# context("count_alive")
# test_that("outputs the same as ctfs::abundance for acaldi", {
#   x <- bci12t7mini
#   acaldi_now <- count_alive_by(x, sp) %>% 
#     dplyr::filter(sp == "acaldi") %>% 
#     pull()
#   
#   acaldi_before <- x %>% 
#     filter(status == "A") %>% 
#     split(x$sp) %>%
#     purrr::map(ctfs::abundance) %>% 
#     purrr::map("abund") %>% 
#     tibble::enframe() %>% 
#     tidyr::unnest()
# 
#   expect_equal(acaldi_now, acaldi_before)



# the difference seems to be where species have 0 counts.
now <- count_alive_by(bci12t7mini, sp) %>% arrange(sp)

before <- ctfs::abundance(bci12t7mini, split1 = bci12t7mini$sp)$abund %>% 
  mutate(sp = rownames(.), n = as.integer(all)) %>% 
  arrange(sp) %>% 
  as_tibble() %>% 
  select(sp = sp, n = n)

all.equal(before[before$n > 0, ], now)

# xxx Maybe I need to change NA by N









