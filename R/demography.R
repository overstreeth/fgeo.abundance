#' Caculate recruitment for all and each species (as in the CTFS R package)
#'
#' `recruitment()` caculates recruitment accross the entire dataset or within 
#' one or two groups and returns a list (as in
#' (http://ctfs.si.edu/Public/CTFSRPackage/). `demography_df()` takes any 
#' demography function and performs the exact same calculation; it works accross
#' the entire dataset or within one group -- not within two groups. And instead
#' of a list it  returns a dataframe, which is easier to manipulate and view 
#' (see examples).
#' 
#' For every individual of all species you must provide two complete datasets,
#' one per census, with variables `dbh`, `pom`, `status` and `date`. Any status
#' indicating a live tree can be submitted in the variable `alivecode`.
#' Survivors are all individuals alive in both censuses, with `status == A` in
#' the first census, and larger than the minimum dbh in the first census. The
#' total population in the second census includes all those alive, above the
#' minimum dbh, plus any other survivors. As in [mortality()], individuals whose
#' status is NA in either census are deleted from all calculations.
#' 
#' @name demography
#' 
#' @family functions from http://ctfs.si.edu/Public/CTFSRPackage/
#' @family functions to compare two censuses
#' 
#' @param .f Demography function.
#' @template census1_census2
#' @template mindbh
#' @template alivecode
#' @param split1 a vector of categories, one per individual
#' @param split2 another vector of categories, one per individual
#' 
#' @return (xxx ask documentation) 
#' * `recruitment()` returns a list with components:
#'   * `N2`:
#'   * `R`:
#'   * `rate`:
#'   * `lower`:
#'   * `upper`:
#'   * `time`:
#'   * `date1`:
#'   * `date2`:
#'
#' * `mortality()` returns a list with components:
#'   * N, the number of individuals alive in the census 1 per category selected
#'   * D, the number of individuals no longer alive in census 2
#'   * rate, the mean annualized mortality rate constant per category selected,
#'     calculated as (log(N)-log(S))/time
#'   * upper, upper confidence limit of mean rate
#'   * lower, lower confidence limit of mean rate
#'   * time, mean time interval in years
#'   * date1, mean date included individuals were measured in census 1, as julian
#'     object (R displays as date, but treats as integer)
#'   * date2, mean date in census 2
#'   * dbhmean, mean dbh in census 1 of individuals included
#'
#' @export
#' @examples
#' # Some data to play with
#' census1 <- forestr::bci12t6mini
#' census2 <- forestr::bci12t7mini
#' 
#' # A bit ackward to manipulate and view  -----------------------------------
#' 
#' recruitment(census1, census2)
#' 
#' each_spp <- recruitment(census1, census2, split1 = census1$sp)
#' # Fit a few species on screen
#' lapply(each_spp, head)
#' 
#' # Easier to manipulate and view -------------------------------------------
#' 
#' # `demography_df()` takes any function but only 1 or NULL splitting variables
#' 
#' split_null <- demography_df(.f = "recruitment", census1, census2)
#' split_null
#' 
#' split_quad <- demography_df(.f = "recruitment", census1, census2, 
#'   split1 = census1$quadrat)
#' head(split_quad, 10)
#' 
#' # Exploring recruitment for some interesting species
#' 
#' split_spp <- demography_df(.f = "recruitment", census1, census2, 
#'   split1 = census1$sp)
#' head(split_spp, 10)
#' 
#' sp_to_explore <- c("casesy", "ocotob", "soroaf")
#' long_format <- subset(split_spp, split1 %in% sp_to_explore)
#' long_format
#' 
#' library(tidyr)
#' wide_format <- tidyr::spread(long_format, key = split1, value = value)
#' wide_format
