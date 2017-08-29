count_alive_by <- function(x, ...) {
  all_sp <- unique(x["sp"])
  alive <- id_alive(x)
  alive_n <- dplyr::count(alive, ...)
  all_sp_n <- dplyr::right_join(alive_n, all_sp)
  tidyr::replace_na(all_sp_n, list(n = 0))
}

count_alive <- function(x) {
  alive_id <- id_alive(x)
  length(unique(alive_id$id))
}

id_alive <- function(x) {
  alive <- x[x$status == "A", ]
  alive$id <- paste0(alive$treeID, "_", alive$stemID)
  alive
}
