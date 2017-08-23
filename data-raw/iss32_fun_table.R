# https://github.com/forestgeo/forestr/issues/32

# This data is on a google sheet owned by Mauro Lepore <maurolepore@gmail.com>
# at https://goo.gl/oiZVho.
# The data were edited by "McMahon, Sean" <mcmahons@si.edu> and Gabriel 
# Arellano <gabriel.arellano.torres@gmail.com>.
# The data were discussed among Sean, Gabriel "Davies, Stuart J." 
# <daviess@si.edu> and Mauro.
# Then, Mauro changed the privilege of Sean and Gabriel over the data so they
# can view it. Should priorities change, Mauro to be notified so he can adapt
# his work accordingly.
# The data were downloaded on the same day, about 30 minutes after the
# discussion finished.

fun_table <- readr::read_csv("./data-raw/iss32_fun_table_edited.csv")

fun_table

use_data(fun_table, internal = TRUE, overwrite = TRUE)
