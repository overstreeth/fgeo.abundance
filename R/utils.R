has_class_df <- function(x) {
  any(grepl("data.frame", class(x)))
}

commas <- function(...) {
  paste0(..., collapse = ", ")
}



# TODO: Move to fgeo.base and @importFrom it --------------------------------

#' Create a vector or dataframe with case insensitive name matching.
#'
#'
#' @section Acknowledgment:
#' Thanks to Neil Richardson for recommending this function
#' (https://twitter.com/enpiar).
#'
#'
#' @param x vector or dataframe to modify.
#' @seealso `httr::insensitive()`.
#' 
#' @author Hadley Wickham (see `httr::insensitive()`).
#' (https://twitter.com/hadleywickham)
#'
#' @keywords internal
#' @export
#' 
#' @examples
#' x <- c("abc" = 1, "def" = 2)
#' x["ABC"]
#' y <- insensitive(x)
#' y["ABC"]
#' y[["ABC"]]
#' y$ABC
#'
#' vft <- data.frame(TreeID = 1)
#' insensitive(vft)[["TreeID"]]
#' insensitive(vft)[["treeid"]]
#' insensitive(vft)["TreeID"]
#' insensitive(vft)["treeid"]
#' insensitive(vft)$TreeID
#' insensitive(vft)$treeid
#'
#' # Works
#' transform(insensitive(vft), new = treeid)
#' # dplyr::mutate(insensitive(vft), new = treeid)
#' # Fails
#' # transform(insensitive(vft), new = TreeID)
#' # dplyr::mutate(insensitive(vft), new = TreeID)
insensitive <- function(x) {
  names(x) <- tolower(names(x))
  structure(x, class = c("insensitive", class(x)))
}

#' @export
`[.insensitive` <- function(x, i, ...) {
  if (is.character(i)) {
    i <- tolower(i)
  }
  
  NextMethod()
}

#' @export
`[[.insensitive` <- `[.insensitive`

#' @export
"$.insensitive" <- function(x, name) {
  name <- tolower(name)
  x[[name]]
}




#' Rename an object based on case-insensitive match of the names of a reference.
#'
#' @param ref Named object to use as reference.
#' @param new New object which names to restored if they match the reference.
#'
#' @return The output is `new` with as many names changed as case-insensitive
#'   matches there are with the reference.
#' @export
#'
#' @examples
#' ref <- data.frame(COLUMN = 1, other = 1)
#' new <- data.frame(Column = 5, Other = 1, n = 5)
#' nms_restore_matching(new, ref)
nms_restore_matching <- function(new, ref) {
  in_ref <- detect_insensitive(names(new), names(ref))
  names(new)[in_ref] <- extract_insensitive(names(new), names(ref))
  new
}



#' Detect and extract matching strings ignoring case.
#' 
#' @param x,y A string.
#' 
#' @keywords internal
#'
#' @return `detect_*` and `extract_*` return a logical vector and a string.
#' @export
#'
#' @examples
#' detect_insensitive(c("StemID", "treeID"), c("stemid", "n"))
#' extract_insensitive(c("StemID", "treeID"), c("stemid", "n"))
#' 
#' vft <- data.frame(TreeID = 1, Status = 1)
#' extract_insensitive(names(vft), tolower(names(vft)))
#' extract_insensitive(tolower(names(vft)), names(vft))
extract_insensitive <- function(x, y) {
  x[detect_insensitive(x, y)]
}
detect_insensitive <- function(x, y) {
  matches <- lapply(fgeo.base::enline(x), grepl, y, ignore.case = TRUE)
  vapply(matches, any, logical(1))
}
