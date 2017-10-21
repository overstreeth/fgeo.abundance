# Examples

library(tidyverse)

census1 <- bciex::bci12t6mini
census2 <- bciex::bci12t7mini
recruitment(census1, census2)

forestr:::recruitment.eachspp(census1, census2) %>% lapply(head)



demography <- function(census1, 
                       census2,
                       mindbh = 10,
                       alivecode = c("A", "AB", "AS"),
                       split1 = NULL, split2 = NULL) {
  r <- recruitment(census1, census2,
    mindbh = mindbh,
    alivecode = alivecode,
    split1 = split1, split2 = split2
  ) %>%
    enframe() %>% 
    unnest()  
  
  m <- mortality(census1, census2,
    alivecode = alivecode, 
    split1 = split1, split2 = split2
  ) %>%
    enframe() %>% 
    unnest()  

  list(recruitment = r, mortality = m) %>% 
    enframe() %>% 
    unnest()  
}

demography(census1, census2)



# assemble.demography -----------------------------------------------------

# This funciton packs results into a dataframe but also does some calculation. 
recruitment(census1, census2) %>% assemble.demography(type = "r")
mortality(census1, census2) %>% assemble.demography(type = "m")

# But this can improve in at least two ways:

# If we are to only create a dataframe, best do this:
recruitment(census1, census2) %>% 
  enframe() %>% 
  unnest()

# If we are oK with the additional calculation, then we may wite
# assemble_demography as a generic method for methods recruitment, mortailty,
# etc. All types are these:
# growth, 
# basal area
# biomass
# abundance
# mortality
# recuitment
