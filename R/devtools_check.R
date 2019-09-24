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
#' @title Check without building vignettes, examples or tests
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

#' @export
#'
#' @title Check and run donttest{} examples
#'
#' @description Runs devtools::check() with appropriate arguments to run
#' donttest{} examples
#'
#' @param pkg description, can be path or package name. See as.package() for
#'  more information
#'
#' @return No return.

check_slow <- function(
  pkg = "."
) {

  devtools::check(
    pkg = pkg,
    args = c('--run-donttest')
  )

}

#' @export
#'
#' @title Check and run donttest{} and dontrun{} examples
#'
#' @description Runs devtools::check() with appropriate arguments to run
#' donttest{} and dontrun{} examples
#'
#' @param pkg description, can be path or package name. See as.package() for
#'  more information
#'
#' @return No return.

check_slower <- function(
  pkg = "."
) {

  devtools::check(
    pkg = pkg,
    args = c('--run-donttest', '--run-dontrun')
  )

}

#' @export
#'
#' @title Check and run donttest{} and dontrun{} examples, max testing
#'
#' @description Runs devtools::check() with appropriate arguments to run
#' donttest{} and dontrun{} examples, maximum testing with things like
#' --use-valgrind
#'
#' @param pkg description, can be path or package name. See as.package() for
#'  more information
#'
#' @return No return.

check_slowest <- function(
  pkg = "."
) {

  devtools::check(
    pkg = pkg,
    args = c('--run-donttest', '--run-dontrun', '--use-valgrind')
  )

}
