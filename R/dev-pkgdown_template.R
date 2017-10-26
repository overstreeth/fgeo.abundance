#' Internal funciton to output the template for the package's website
#' 
#' Internal funciton to output the template for package website built with 
#' the __pkgdown__ package (http://hadley.github.io/pkgdown/).
#' 
#' Does not overwrite existing file.
#' 
#' @return Outputs the file `_pkgdown.yml` (in working directory by default).
#' @examples
#' \dontrun{
#' write_pkgdown_template()
#' }
#'
#' @param dir Directory where the file `pkgdown.yml` will be placed.
#' 
#' @seealso [pkgdown::build_site()]
#' 
#' @keywords internal
#' @family functions for developers
write_pkgdown_template <- function(dir = "./") {
  file <- paste0(dir, "_pkgdown.yml")
  if (file.exists(file)) {
    stop(file, " already exists. Are you sure you want to overwrite it?\n",
      "  * For safety, first remove existing file manually; then retry")
  }
  message("Writting ", file, "\n",
    "  * Edit the 'reference' and 'articles' fields labelled with 'xxx';\n",
    "  * Remove unused fields.\n",
    "(To change the template overwrite `pkgdown_template` in sysdata.rda.)")
  readr::write_lines(pkgdown_template, file)
  file.edit(file)
}
