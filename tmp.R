# TODO

# Remove count_woods() or raplace it by a more direct function that uses checks
# for unique main stems, then uses filter, then count_unique_treeid(). It sould
# be the way to calculate abundance. It sould be closely related to basal_area.
# The implementaion should look like:
# for grouped data:
# pick_largest_hom_dbh()
# filter()
# count_distinct_treeid() or basal_area()



# Refactoring to pick_largest_hom_dbh().

# byyr could be simpler if I expect class "main_stem" built with main_stem() or 
# as_main_stem(), which should construct objects based on pick_largest_hom_dbh()


# Deliver to Suzanne



# SUZANNE REQUESTS

# Test this is all true. (From Suzanne). Maybe add test of unique stem id by censuid and tree id after output of pick_largest_hom_dbh

# In each census (plotcensusnumber):
#   
#   1.       Make sure that there are no duplicate StemIDs. If there are duplicate StemIDs in one census, choose the one with the largest HOM.
# 
# Note: The reason for this in BCI, is that stems with buttresses may be measured at more than one height along the stem. You have to choose the dbh with the HIGHEST HOM (even if it is not the largest dbh).
# 
# 2.       Once you have a census with unique StemIDS, you can now make a table with unique TreeIDs.
#
# 3.       For each TreeID, get the mnemonic and select the stemID with the largest DBH of each TreeID.
# 
# 4.       Now you can count the trees for each mnemonic for dbh>=10 and dbh>=100.
