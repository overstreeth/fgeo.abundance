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




# TODO

# byyr could be simpler if I expect class "main_stem" built with main_stem() or 
# as_main_stem(), which should construct objects based on pick_largest_hom_dbh()

# In pick_woods(), force expressions to be of dbh: Add something like:
# (From fgeo.abundance)
# Then remove it from elsewhere downstream.
dots <- lowercase_var(..., .var = "dbh")
flag_if_not_expression_of_var(dots, .flag = rlang::abort, .var = "dbh")




# Bring pick_woods_f to fgeo.abundance and refactor if necessary.

# Make count_distinct_treeid internal?
# In count_woods() incorporate code and lessons learned from *byr().
# * correct for buttressess.
# * enforce `...` to take expressions of dbh. Reflect this in the documentation.

# Once the code now in byyr() is incorporated into count_woods() then I can use
# count_woods() directly into byyr().

# Clearly document that count_wods() is the way to calculate abundance.

# Think if basal_area() needs to internally count_woods() (the aspirational,
# improved version)












# Rerun Suzanne's funcitons
# Make tests shorter to run


# TODO: Think how to deal with status in combination with the pick() functions. It looks like I should first determine the status of a tree. Then remove dead stems, then pick, etc.

Use internal used in building pick_dhh_largest() to write fun to pick_hom_largest() by census is treeid by stemid. Err if multiple plots are detected.

Use this inside count_woods(), right before pick_dbh_largest().


Extract factory pick_var_by_group_by_censusid()

function(var, group) {
  function(.x, .arrange, .pick) {
    # code
  }
}

pick_hom_by_stemid_by_censusid <-
  pick_var_by_group_by_censusid(hom, stemid)


pick_hom_largest <- function(.x) {
  pick_hom_by_stemid_by_censusid(
    .x, 
    .arrange = dplyr::desc, 
    .pick = row_number() == 1
  )
}




asap

