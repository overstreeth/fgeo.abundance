count_alive <- function(x, .status = "A", ...) {
  if (missing(...)) {
    return(count_status(x = x, .status = .status))
  }
  
  count_status_by(x = x, .status = .status, ...)
}

count_status_by <- function(x, .status, ...) {
  # To later add species excluded by status, so that their count is cero
  all_sp <- unique(x["sp"])
  
  identified <- add_id(x)
  filtered <- filter_status(x = identified, .status = .status)
  grouped_summary <- dplyr::count(filtered, ...)
  # dplyr::count(filtered, sp)
  
  # to excluded species give a count of cero
  all_sp_n <- dplyr::right_join(grouped_summary, all_sp)
  tidyr::replace_na(all_sp_n, list(n = 0))
}

count_status <- function(x, .status) {
  identified <- add_id(x)
  filtered <- filter_status(x = identified, .status = .status)
  dplyr::tally(filtered)[["n"]]
}

add_id <- function(x, .status) {
  x$id <- paste0(x$treeID, "_", x$stemID)
  x
}

filter_status <- function(x, .status) {
  stopifnot(all(.status %in% unique(x$status)))
  x[x$status %in% .status, ]
}
