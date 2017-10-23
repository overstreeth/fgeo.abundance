# Some data to play with
census1 <- forestr::bci12t6mini
census2 <- forestr::bci12t7mini

recruitment(census1, census2)
each_spp <- recruitment(census1, census2, split1 = census1$sp)
# Showing results for only a few species with `head()`
lapply(each_spp, head)

mortality(census1, census2)
each_spp <- mortality(census1, census2, split1 = census1$sp)
# Showing results for only a few species with `head()`
lapply(each_spp, head)

# Easier to manipulate and view

split_null <- recruitment_df(census1, census2)
split_null

split_quad <- recruitment_df(census1, census2, split1 = census1$quadrat)
head(split_quad, 10)

# Exploring recruitment for some interesting species

split_spp <- recruitment_df(census1, census2, split1 = census1$sp)
head(split_spp, 10)




sp_to_explore <- sample(unique(split_spp$split), 3)
long_format <- subset(split_spp, split %in% sp_to_explore)
head(long_format, 10)

library(tidyr)
wide_format <- tidyr::spread(long_format, key = split, value = value)
wide_format

