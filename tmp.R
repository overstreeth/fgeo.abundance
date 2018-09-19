# TODO

# * Remove fgeo.tool::pick_woods_f()?
# * Show abundance on the result of pick_woods()?

# * Remove count_distinct() and friends? Or replace it by a form reusing code used in abundance() and basal_area()?






# * Check TODO and FIXME
# * Make interface equal for count_distinct() and basal_area().
# * Remove count_woods() or raplace it by a more direct function that uses checks
#   for unique main stems, then uses filter, then count_unique_treeid(). It sould
#   be the way to calculate abundance. It sould be closely related to basal_area.
#   The implementaion should look like:
#   for grouped data:
#   pick_main_stem()
#   filter()
#   count_distinct_treeid() or basal_area()
# * byyr could be simpler if I expect class "main_stem" built with main_stem() or 
# as_main_stem(), which should construct objects based on pick_main_stem()
