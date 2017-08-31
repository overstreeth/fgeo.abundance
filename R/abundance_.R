#' Count individuals
#'
#' @template x 
#' @param group_by Sting giving the variables to group by.
#' @param only.alive 
#'
#' @return
#' @export
#'
#' @examples
#' stem <- bci12t7mini
#' (n <- abundance_n(stem))
#' class(n)
#' 
#' abundance_n(stem, group_by = "sp", F)
abundance_n <- function(x, group_by = c("quadrat", "sp"), only.alive = TRUE) {
{
  if(only.alive) {valid.status <- "A"} else {valid.status <- unique(x$status)}
  table(x[x$status %in% valid.status, group_by])
}

abundance_n_plyr <- function(x, group_by = c("sp", "status"), alive_only = TRUE) {
  n <- plyr::ddply(
    .data = x, 
    .variables = group_by, 
    .fun = function(x) {length(unique(x))}
  )
  
  if (!alive_only) {
    return(n)
  }
  n[n$status == "A", ]
}



abundance_n_dplyr <- function(x, ..., only_alive = TRUE) {
  if (!only_alive) {
    x <- x[x$status == "A", ]
  }
  
  group_by <- rlang::quos(...)

  grouped <- dplyr::group_by(x, !!!group_by)
  dplyr::ungroup(dplyr::summarise(grouped, n = n()))
}

abundance_n_dplyr(stem)
abundance_n_dplyr(stem, status, sp, only_alive = FALSE)

