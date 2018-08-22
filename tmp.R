# TODO

# * Make interface equal for count_distinct() and basal_area().
# * Implement add_ via left_join()

# Add a progress bar to pick_main_stem()?
# Speed pick_main_stem() with data.table()?


# Remove count_woods() or raplace it by a more direct function that uses checks
# for unique main stems, then uses filter, then count_unique_treeid(). It sould
# be the way to calculate abundance. It sould be closely related to basal_area.
# The implementaion should look like:
# for grouped data:
# pick_main_stem()
# filter()
# count_distinct_treeid() or basal_area()

# byyr could be simpler if I expect class "main_stem" built with main_stem() or 
# as_main_stem(), which should construct objects based on pick_main_stem()
