library(tidyverse)

# Some data to play with
census1 <- forestr::bci12t6mini
census2 <- forestr::bci12t7mini

census1$census <- "census1"
census2$census <- "census2"

bind_rows(census1, census2) %>% 
  as_tibble() %>%
  nest(-sp, -quadrat, -census) %>% 
  spread(census, data) %>% 
  mutate(recru = map2(census1, census2, recruitment)) %>% 
  select(-census1, -census2) %>% 
  mutate(recru = map(recru, enframe)) %>% 
  unnest() %>% 
  unnest()

demography_df("recruitment", census1, census2, split1 = census1$sp)
  
  group_by(census, sp) %>% 
  summarise(recruitment = function(x, y){
    list(map)
  })
  










