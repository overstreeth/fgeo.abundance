census1 <- forestr::bci12t6mini
census2 <- forestr::bci12t7mini


out6 <- recruitment(census1, census2)
cns1_na <- census1
cns1_na[] <- NA
recruitment(cns1_na, census2)

cns1_null <- census1
cns1_null[] <- NULL
purrr::is_empty(cns1_null)
recruitment(cns1_null, census2)

cns1_empty <- dplyr::filter(census1, sp %in% "hello")  # no sp is called hello
purrr::is_empty(cns1_empty)
recruitment(cns1_empty, census2)




# must check if dbh is na

# for one spp with data works fine
cns1_6 <- census1 %>% dplyr::filter(sp == "herrpu")
cns2_6 <- census2 %>% dplyr::filter(sp == "herrpu")
recruitment(cns1_6, cns2_6)

# does not warn if dbh is na for a sppecies
# one spp is all na
na_sp1 <- dplyr::mutate(census1, dbh = ifelse(sp == "cordal", NA, dbh))
na_sp2 <- dplyr::mutate(census1, dbh = ifelse(sp == "cordal", NA, dbh))
recruitment(na_sp1, na_sp2)



