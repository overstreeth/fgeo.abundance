recruitment_by <- function(census1, 
                           census2, 
                           group_by = c("sp"),
                           only_alive = TRUE, ...) {
  # Reformat data to map each group to a data set in census1 and census2
  group_vars <- group_by
  census_df <- list(census1 = census1, census2 = census2) %>%
    # map(as.tibble) %>%
    enframe(name = "census") %>%
    unnest()
  grouped <- group(x = census_df, group_by = c("census", group_vars),
    only_alive = only_alive)
  nested <- nest(grouped)
  .spread <- spread(nested, census, data)

  # Remove spcecies with at least one empty data set
  is_empty1 <- unlist(map(.spread$census2, is_empty))
  is_empty2 <- unlist(map(.spread$census1, is_empty))
  cns_non_empty <- mutate(.spread, 
    is_empty1 = is_empty1, 
    is_empty2 = is_empty2,
    are_both_non_empty = !(is_empty1 | is_empty2)
  ) %>% 
    filter(are_both_non_empty) %>% 
    select(-is_empty1, -is_empty2, -are_both_non_empty)
  
  # Calculate recruitment for each data set
  recruitment <- mutate(cns_non_empty, 
    recruitment = map2(census1, census2, recruitment_df))
  unnest(recruitment[c(group_vars, "recruitment")])
}

  
# list(census1 = census1, census2 = census2) %>% 
# # map(as.tibble) %>% 
# enframe(name = "census") %>% 
# unnest() %>% 
# group_by(census, sp) %>% 
# nest() %>% 
# spread(census, data) %>% 
# mutate(recruitment = map2(census1, census2, recruitment_df))












