# TODO

# * Explore issue rose by Suzanne by which species with buttresses are higher
#   than she expected (and calculated), particularly for census 3, and also from
#   census 3 onwards.
# * Check if regression test of output of _byyr is the same before and after
#   refactoring `basal_area()` (notice that the formula changed).
# * Why is the formula of basal area different in my code and Condit's code?
# * Check TODO and FIXME
# * Remove add_basal_area() ? and tally...()? to keep things simple or implement
#   it via join?


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
