# TODO

# Make count_distinct_treeid internal?

# Reuse code and documentation from fgeo.tool::pick_largest_hom_dbh in
# fgeo.tool::pick_woods (document them together?), and in
# fgeo.abundance::*byyr() in fgeo.abundance::count_woods().


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

