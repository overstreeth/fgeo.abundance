# Filtering only 200 trees to make the data small
few_trees <- sample(unique(bci12s6mini$tag), 200)
census1 <- subset(bci12s6mini, tag %in% few_trees)
census2 <- subset(bci12s7mini, tag %in% few_trees)

growth(census1, census2)

# View just a few species with `head()`
lapply(growth(census1, census2, split1 = census1$sp), head)

split_by_two <- growth(census1, census2, 
  split1 = census1$sp, split2 = census1$quadrat)
lapply(split_by_two, function(x) head(x[1:6]))
str(split_by_two)

