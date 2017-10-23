#' Caculate recruitment for all and each species (as in the CTFS R package)
#'
#' Caculate `recruitment()`, `mortality()` and `growth()` accross the entire
#' dataset or within one or two groups (as in
#' (http://ctfs.si.edu/Public/CTFSRPackage/). Each of these demography functions
#' has a `*_df()` version that accepts not more than one split variable and,
#' instead of a list, returns a dataframe.
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
#' @family functions from http://ctfs.si.edu/Public/CTFSRPackage/
#' @family functions to compare two censuses
#' 
#' @template census1_census2
#' @template mindbh
#' @template alivecode
#' @param split1 a vector of categories, one per individual
#' @param split2 another vector of categories, one per individual
#' 
#' @return (xxx ask documentation) 
#' `recruitment()` returns a list with components:
#' * `N2`:
#' * `R`:
#' * `rate`:
#' * `lower`:
#' * `upper`:
#' * `time`:
#' * `date1`:
#' * `date2`:
#'
#' `mortality()` returns a list with components:
#' * `N`: the number of individuals alive in the census 1 per category selected
#' * `D`: the number of individuals no longer alive in census 2
#' * `rate`: the mean annualized mortality rate constant per category selected,
#'   calculated as (log(N)-log(S))/time
#' * `upper`: upper confidence limit of mean rate
#' * `lower`: lower confidence limit of mean rate
#' * `time`: mean time interval in years
#' * `date1`: mean date included individuals were measured in census 1, as
#'   julian object (R displays as date, but treats as integer)
#' * `date2`: mean date in census 2
#' * `dbhmean`: mean dbh in census 1 of individuals included
#'
#' @examples
#' # Some data to play with
#' census1 <- forestr::bci12t6mini
#' census2 <- forestr::bci12t7mini
#' 
#' recruitment(census1, census2)
#' 
#' # Easier to view and filter when you split by some variable with many groups
#' # (note warning when some groups have dbh values full of NA)
#' recruitment_df(census1, census2, split1 = census1$sp)
#' 
#' # Same for `mortality()`; lets explore a few species
#' (all_sp <- mortality_df(census1, census2, split1 = census1$sp))
#' few_sp <- sample(unique(all_sp$split), 3)
#' long_format <- subset(all_sp, split %in% few_sp)
#' head(long_format, 10)
#' 
#' # From long to wide format
#' library(tidyr)
#' wide_format <- tidyr::spread(long_format, key = split, value = value)
#' wide_format
#' @name demography
NULL



# Internal helper of recruitment_df, mortality_df and growth_df
demography_df <- function(demography_list) {
  # Transform result from list to dataframe
  nested <- tibble::enframe(demography_list, "metric")
  unnested <- tidyr::unnest(
    dplyr::mutate(nested, value = purrr::map(.data$value, tibble::enframe))
  )
  # Reorganize columns
  df_long <- dplyr::select(unnested, .data$name, .data$metric, .data$value)
  arranged <- dplyr::arrange(df_long, .data$name)
  dplyr::rename(arranged, split = .data$name)
}

