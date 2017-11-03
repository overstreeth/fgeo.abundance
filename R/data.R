#' Subset of tree data from Barro Colorado Island, Panama.
#'
#' A subset of data of 1000 randomly selected trees from Barro Colorado Island,
#' Panama (data released in 2012). For every bciYYtNmini, "YY" and "N" indicate
#' the year and census number. For example, bci12t1mini contains BCI data
#' released in 2012 from census 1.
#'
#' @source From \url{https://repository.si.edu/handle/10088/20925}, file
#'   bci.full#.rda.
#'
#' @format A dataframe with 1000 rows and 20 variables.
#' Variables are defined at: http://ctfs.si.edu/Public/DataDict/data_dict.php.
#' @aliases bci12t1mini bci12t2mini bci12t3mini bci12t4mini bci12t5mini
#'   bci12t6mini bci12t7mini
#' @name bciYYtNmini
NULL



#' Subset of stem data from Barro Colorado Island, Panama.
#'
#' Stem data corresponding to a subset of 1000 randomly selected trees from
#' Barro Colorado Island, Panama (data released in 2012). For every bciYYsNmini,
#' "YY" and "N" indicate the year and census number. For example, bci12s1mini
#' contains BCI data released in 2012 from census 1.
#'
#' @source From \url{https://repository.si.edu/handle/10088/20925}, file
#'   bci.stem#.rda.
#'
#' @format A dataframe with 2165 rows and 20 variables.
#' Variables are defined at: http://ctfs.si.edu/Public/DataDict/data_dict.php.
#' @aliases bci12s1mini bci12s2mini bci12s3mini bci12s4mini bci12s5mini
#'   bci12s6mini bci12s7mini
#' @name bciYYsNmini
NULL


# Elevation Data ----------------------------------------------------------

#' Elevation data from Barro Colorado Island, Panama, recorded over a 5x5m grid.
#'
#' Elevation data from Barro Colorado Island (BCI), Panama, recorded over a 5x5m
#' grid.
#'
#' @source Lao, Suzanne \email{LAOZ@@si.edu}.
#' @format
#' A tibble with 20301 rows and 3 variables:
#' * `x`, `y`: coordinates at every corner across the BCI plot (xxx which
#' corner?).
#' * `elev`: elevation at every corner across the plot (xxx which corner?, xxx
#' is the unit meters?).
"bci_elevation"
