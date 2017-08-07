# spatial; quadfunc -------------------------------------------------------

#' Takes row and column numbers and identifies the quadrate number (index).
#'
#' @description
#' From row and column numbers (based on `gridsize`) outputs the corresponding
#' quadrat number, i.e. index (id).
#' 
#' @param row_num Row number.
#' @param col_num Column number.
#' @template gridsize
#' @template plotdim
#' 
#' @aliases rowcol.to.index
#' @family converters
#' 
#' @export
#'
to_id_rowcol <- function(row_num, 
                         col_num, 
                         gridsize, 
                         plotdim) {
  stopifnot(gridsize > 0)
  stopifnot(all(plotdim > 0))
  stopifnot(assertive::is_non_empty(row_num))
  stopifnot(assertive::is_non_empty(col_num))
  if (anyNA(row_num)) {warning("`rowno` and output contain NA or NaN.")}
  if (anyNA(col_num)) {warning("`colno` and output contain NA or NaN.")}
  if (any(is.infinite(row_num))) {warning("`rowno` contains infinite values.")}
  if (any(is.infinite(col_num))) {warning("`rowno` contains infinite values.")}
  
  badrc = (row_num <= 0 | col_num <= 0 | row_num > plotdim[2]/gridsize | 
    col_num > plotdim[1]/gridsize)
  row_num = row_num - 1
  col_num = col_num - 1
  maxrow = floor(plotdim[2]/gridsize)
  index = col_num * maxrow + row_num + 1
  
  if (length(badrc[badrc > 0])) 
    index[badrc] = NA
  
  return(index)
}
