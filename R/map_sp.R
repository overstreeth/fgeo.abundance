# Map species from a census.

#' Map the distribution of one, some or all species in a census data set.
#'
#' @param census Census data.
#' @param species A string of the species codes to plot (`sp`).
#' @param file A character string giving the name of the file.
#' @param ... Arguments passed to [ggplot2::geom_layer()] via
#'   [ggplot2::geom_point()].
#'
#' @seealso [ggplot2::geom_layer()], [grDevices::pdf()], [grDevices::png()].
#'
#' @section Acknowledgement:
#' Thanks to Gabriel Arellano and David Kenfack for ideas and feedback.
#'
#' @return A list of plots is always returned:
#' * `map_sp()` returns it visibly;
#' * `map_sp_pdf()` returns it invisibly so it can be reused, but its main
#' @export
#'
#' @examples
#' \dontrun{
#' library(dplyr)  # to make it easer to manipulate data
#' library(gridExtra)  # to arrange multiple plots in one page
#'
#' # Data for example
#' census <- forestr::bci12t7mini
#'
#' # Plot to screen
#' map_sp(census, "hybapr")
#'
#' # Tweak plot with arguments passed to [ggplot2::layer()]
#' map_sp(census, c("hybapr", "faraoc"), color = "blue", size = 3, shape = 1)
#'
#' # Selecting only a few abundant species
#' count_sp <- dplyr::count(census, sp)
#' decreasing_n <- dplyr::arrange(count_sp, desc(n))
#' top_n <- decreasing_n$sp[1:8]
#'
#' # Plot to screen
#' one_per_page <- map_sp(census, top_n)
#' one_per_page
#'
#' # Print multiple page in a single .pdf file.
#' # Invisibly returning a list of plots that can be reused
#' one_per_page <- map_sp_pdf(census, top_n)
#'
#' # To export .png wrap your plots between png() and dev.off()
#' png()
#' one_per_page[1:3]
#' dev.off()
#'
#' # Reusing one_per_page
#' multi_per_page <- gridExtra::marrangeGrob(one_per_page, ncol = 2, nrow = 2)
#' multi_per_page
#'
#' # Saving the multipage into a .pdf
#' pdf("multi_paged.pdf", paper = "letter")
#' multi_per_page
#' dev.off()
#' }
map_sp <- function(census, species, ...) {
  check_crucial_names(census, c("gx", "gy", "sp"))
  assertive::assert_is_character(species)
  assertive::assert_is_non_empty(species)

  plots <- lapply(X = species, FUN = map_one_sp, census = census, ...)
  names(plots) <- species
  plots
}

#' @export
#' @rdname map_sp
map_sp_pdf <- function(census, species, file = "map.pdf", ...) {
  on.exit(dev.off())
  plots <- map_sp(census = census, species = species, ...)
  pdf(file = file)
  invisible(lapply(plots, print))
  message("Saving maps as ", file)
  invisible(plots)
}

# General plot of gx by gy faceted by species.
map_xy <- function(census, xlim, ylim, ...) {
  ggplot2::ggplot(
    data = census,
    ggplot2::aes(x = census$gx, y = census$gy)
  ) +
    ggplot2::geom_point(...) +
    ggplot2::facet_grid(. ~ census$sp) +
    ggplot2::coord_fixed(xlim = xlim, ylim = ylim) +
    ggplot2::labs(x = NULL, y = NULL) +
    ggplot2::theme_bw()
}

# Standarized plot for each species (fixed ratio and limits).
map_one_sp <- function(census, one_sp, ...) {
  xlim <- c(0, max(census$gx, na.rm = TRUE))
  ylim <- c(0, max(census$gy, na.rm = TRUE))
  assertive::assert_all_are_not_na(c(xlim, ylim))

  filtered_census <- census[census$sp %in% one_sp, ]
  map_xy(filtered_census, xlim, ylim, ...)
}
