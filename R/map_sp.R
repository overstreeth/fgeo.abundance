# Map species from a census.

#' Map the distribution of one, some or all species in a census data set.
#' 
#' Only the first two arguments are strictly necessary and the defaults are set
#' to cover most common cases. All other arguments let you customize your map:
#' they let you customize the points; the plot theme; add your elevation data, 
#' and customize the elevation lines.
#' 
#' @param census Census data.
#' @param species A string of the species codes to plot (`sp`).
#' @param xlim,ylim A vector giving the limits of x, y axes, for example 
#'   `xlim = c(0, 1000), ylim = c(0, 500)`. Default limits should be OK -- they
#'   are set to be c(0, max), where max is the maximum value of `gx` or `gy`
#'   in the data set.
#' @param theme A ggplot2 theme to customize the looks of the map.
#' @param elevation A dataframe with variables gx, gy, and elev giving the 
#'   elevation of the site.
#' @param line_size A number to customize the width of the elevation lines.
#' @param low,high Colours to represent the range between low and high 
#'   elevation. Use colour names like `low = "black", high = "red"` or HEX
#'   colours like `low = "#132B43", high = "#56B1F7"` (for more colours google 
#'   #132B43).
#' @param bins A number. Setting bins creates evenly spaced contours in the
#'   range of the data. Integers 
#' @param file A character string giving the name of the file.
#' @param ... Arguments passed to [ggplot2::geom_layer()] via
#'   [ggplot2::geom_point()] to customize, for example, the size, shape,
#'   or colour of the points.
#'
#' @seealso [ggplot2::geom_layer()], [grDevices::pdf()], [grDevices::png()], 
#'   [ggplot2::theme()].
#'
#' @section Acknowledgement:
#' Thanks to Gabriel Arellano and David Kenfack for ideas and feedback.
#'
#' @return Both functions return a list of plots. `map_sp()` returns it visibly;
#'   `map_sp_pdf()` returns it invisibly so it can be reused, but its main
#'   output is a .pdf file.
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
#'
#'
#'
#' # ERRORS AND WARNINGS
#' 
#' # Match exactly the names of crucial variables
#' census <- dplyr::rename(census, SP = sp)
#' # This fails
#' map_sp(census, species = "hybapr")
#'
#' # File extension should be .pdf
#' map_sp_pdf(census, top_n, file = "extention_good.pdf")  # ok
#' map_sp_pdf(census, top_n, file = "extention_bad")  # replaced by default
#' }
map_sp <- function(census,
                   species,
                   xlim = NULL,
                   ylim = NULL,
                   theme = ggplot2::theme_bw(),
                   elevation = NULL,
                   line_size = 0.5,
                   low = "#132B43",
                   high = "#56B1F7",
                   bins = NULL,
                   ...) {
  check_map_sp(census, species)
  
  plots <- lapply(X = species, FUN = map_one_sp, census = census, 
    xlim = xlim, ylim = ylim, theme = theme,
    elevation = elevation, line_size = line_size, low = low, high = high,
    bins = bins, ...)
  names(plots) <- species
  print(plots)
}

#' @export
#' @rdname map_sp
map_sp_pdf <- function(census,
                       species,
                       xlim = NULL,
                       ylim = NULL,
                       theme = ggplot2::theme_bw(),
                       elevation = NULL,
                       line_size = 0.5,
                       low = "#132B43",
                       high = "#56B1F7",
                       bins = NULL,
                       file = "map.pdf",
                       ...) {
  check_map_sp(census, species)
  file <- check_file_extension(file)
  
  plots <- map_sp(census = census, species = species, 
    xlim = xlim, ylim = ylim, theme = theme, 
    elevation = elevation, line_size = line_size, low = low, high = high, 
    bins = bins, ...)
  pdf(file = file)
  on.exit(dev.off())
  invisible(lapply(plots, print))
  message("Saving as ", file)
  
  invisible(plots)
}

#' Standarized plot for each species (fixed ratio and limits).
#' @noRd
map_one_sp <- function(census,
                       one_sp,
                       xlim = NULL,
                       ylim = NULL,
                       theme = ggplot2::theme_bw(),
                       elevation = NULL,
                       line_size = 0.5,
                       low = "#132B43",
                       high = "#56B1F7",
                       bins = NULL,
                       ...) {
  assertive::assert_is_character(one_sp)
  assertive::assert_is_of_length(one_sp, 1)
  
  if (is.null(xlim)) {xlim <- c(0, max(census$gx, na.rm = TRUE))}
  if (is.null(ylim)) {ylim <- c(0, max(census$gy, na.rm = TRUE))}

  filtered_census <- census[census$sp %in% one_sp, ]
  p <- map_basic(filtered_census, xlim, ylim, theme = theme, ...)
  if (!is.null(elevation)) {
    p <- add_elevation(ggplot = p, elevation = elevation, line_size = line_size,
      low = low, high = high, bins = bins)
  }
  p
}

#' General plot of gx by gy faceted by species.
#' @noRd
map_basic <- function(census, xlim, ylim, theme = ggplot2::theme_bw(), ...) {
  check_crucial_names(census, c("gx", "gy"))
  assertive::assert_all_are_not_na(c(xlim, ylim))

  ggplot2::ggplot(
    data = census,
    ggplot2::aes(x = gx, y = gy)
  ) +
    ggplot2::geom_point(...) +
    ggplot2::facet_grid(. ~ sp) +
    ggplot2::coord_fixed(xlim = xlim, ylim = ylim) +
    ggplot2::labs(x = NULL, y = NULL) +
    theme
}

#' Add elevation lines to a ggplot.
#' @noRd
add_elevation <- function(ggplot,
                          elevation,
                          line_size = 0.5,
                          low = "#132B43",
                          high = "#56B1F7",
                          bins = NULL) {
  base_plot_is_class_ggplot <- any(grepl("ggplot", class(ggplot)))
  stopifnot(base_plot_is_class_ggplot)
  elevation_is_dataframe <- any(grepl("data.frame", class(elevation)))
  stopifnot(elevation_is_dataframe)
  check_crucial_names(elevation, c("gx", "gy", "elev"))

  p <- ggplot + 
    ggplot2::stat_contour(data = elevation, 
      ggplot2::aes(x = gx, y = gy, z = elev, colour = ..level..), 
      size = line_size, bins = bins) +
    ggplot2::scale_colour_continuous(low = low, high = high)
  labels_properties <- list("far.from.others.borders", "calc.boxes", 
    "enlarge.box", box.color = NA, fill = "transparent", "draw.rects")
  p_with_labels <- directlabels::direct.label(p, labels_properties)
  p_with_labels
}
