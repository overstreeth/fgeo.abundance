#' Abundance.
#' 
#' * Copy of [dplyr::count()].
#' * Supports groups.
#' * Returns 0 when fed with a 0-row dataframe.
#' 
#' [fgeo.abundance::abundance()] versus ctfs::abundance():
#' * When fed with a 0-row dataframe, 
#'     * `ctfs::abundance` returns a dataframe with zero rows and zero columns. 
#'     * [fgeo.abundance::abundance()] returns a dataframe column `n = 0`.
#' * `ctfs::abundance` handles only two grouping variables, via arguments
#' `split1` and `split2`.
#' * [fgeo.abundance::abundance()] handles any number of grouping variables via
#' [dplyr::group_by()].
#' 
#' * `ctfs::abundance` returns a list, which is difficult to visualize and use.
#' * [fgeo.abundance::abundance()] returns a dataframe subclass tibble, which
#' prints well even with large datasets and works well with a larger number of
#' tools for data analysis (e.g. __ggplot2__ and __dplyr__).
#' 
#' @section Flags:
#' Trows warning if detects multiple
#' stems, TODO
#' censuses,  TODO
#' plots,  TODO
#' 
#' 
#' @section Warning:
#' To pick specific rows (e.g. to pick "alive" stems or `dbh` within some range)
#' you should use functions such as `[`, [subset()], or [dplyr::filter()]
#' _before_ using `abundance()`. (Notice the difference with `ctfs::abundance`,
#' which includes arguments to deal with `dbh` and `status`).
#' 
#' @name abundance
NULL

#' @rdname abundance
abundance <- dplyr::count

