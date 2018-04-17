library(dplyr)
library(ggplot2)

cns <- data.frame(
  CensusID = factor(rep(c(1, 2), 3)),
  quadrat = "0000",
  sp = rep(paste0("sp", 1:3), 2),
  n = sample.int(100, 6)
)
cns

smry_diversity(cns, n)

# The output of `smry_diversity` flows well into common pipelines:

diversity <- cns %>%
  group_by(CensusID) %>%
  smry_diversity(n)
# Same
diversity <- group_by(cns, CensusID)
diversity <- smry_diversity(diversity, n)

diversity

# A plot
diversity %>%
  ggplot(aes(CensusID, value)) +
  geom_col(aes(fill = diversity), position = "dodge")
# A summary
diversity %>%
  group_by(diversity) %>%
  summarise(mean_diversity = mean(value))
