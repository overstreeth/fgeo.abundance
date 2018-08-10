# TODO: Review byyr in debugger to identidy potential refactorings.

library(fgeo)

has_dead <- function(vft) {
  any("dead" %in% unique(vft[["Status"]]))
}

vft <- fgeo.data::luquillo_vft_4quad
has_dead(vft)

vft_plot <- pick_plotname(vft, "luquillo")
has_dead(vft_plot)

one_stem_per_treeid <- pick_largest_hom_dbh(vft_plot)
has_dead(one_stem_per_treeid)

# This filters on dbh, so it'll remove missing dbh and therefore dead trees
stems <- pick_trees(one_stem_per_treeid)
has_dead(stems)

abundance_byyr(stems)




# Standardize basal area by total plot-hectares.
standardize_ba <- function(ba, denominator) {
  years <- setdiff(names(ba), c("species", "Family"))
  in_he <- convert_unit_at(ba, .at = years, from = "mm2", to = "m2")
  standardize_at(in_he, years, denominator)
}

create_all_tables <- function(vft, plotname, denominator) {
  tree_abund <- create_table(vft, plotname, pick_trees, abundance_byyr)
  sapling_abund <- create_table(vft, plotname, pick_saplings, abundance_byyr)
  tree_basal <- create_table(vft, plotname, pick_trees, basal_area_byyr) %>% 
    standardize_ba(denominator)



# TODO: Use or remove fix_status_if_bad_or_err().
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

