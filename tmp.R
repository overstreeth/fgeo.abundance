census1 <- forestr::bci12t6mini
census2 <- forestr::bci12t7mini

recruitment_by(census1, census2, group_by = c("quadrat"))
