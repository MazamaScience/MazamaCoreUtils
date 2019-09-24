#' @export
#'
#' @title Check without building vignettes
#'
#' @description Runs devtools::check() with appropriate arguments to avoid
#' building or checking vignettes.
#'
#' @param pkg description, can be path or package name. See as.package() for
#'  more information
#'
#' @return No return.

check_fast <- function(
  pkg = "."
) {

  devtools::check(
    pkg = pkg,
    build_args = c("--no-build-vignettes"),
    args = c('--ignore-vignettes')
  )

}

#' @export
#'
#' @title Check without building vignettes or examples
#'
#' @description Runs devtools::check() with appropriate arguments to avoid
#' building or checking vignettes or examples.
#'
#' @param pkg description, can be path or package name. See as.package() for
#'  more information
#'
#' @return No return.

check_faster <- function(
  pkg = "."
) {

  devtools::check(
    pkg = pkg,
    build_args = c("--no-build-vignettes"),
    args = c('--ignore-vignettes', '--no-examples')
  )

}

#' @export
#'
#' @title Check without building vignettes or examples
#'
#' @description Runs devtools::check() with appropriate arguments to avoid
#' documentation rebuild, vignettes, examples and tests.
#'
#' @param pkg description, can be path or package name. See as.package() for
#'  more information
#'
#' @return No return.

check_fastest <- function(
  pkg = "."
) {

  devtools::check(
    pkg = pkg,
    build_args = c('--no-build-vignettes'),
    args = c('--ignore-vignettes','--no-tests', '--no-examples', '--no-manual')
  )

}
