#' @export
#'
#' @title Package check
#'
#' @description Runs devtools::check() with no additional arguments.
#' This is a wrapper for:
#'
#' \preformatted{
#' devtools::check()
#' }
#'
#' @param pkg passed to \code{devtools::check()}
#'
#' @return No return.

check <- function(
  pkg = "."
) {

  devtools::check(
    pkg = pkg
  )

}

#' @export
#'
#' @title Package check without building vignettes
#'
#' @description Runs devtools::check() with appropriate arguments to avoid
#' building or checking vignettes.
#' This is a wrapper for:
#'
#' \preformatted{
#' devtools::check(
#'   build_args = c("--no-build-vignettes"),
#'   args = c("--ignore-vignettes")
#' )
#' }
#'
#' @param pkg passed to \code{devtools::check()}
#'
#' @return No return.

check_fast <- function(
  pkg = "."
) {

  devtools::check(
    pkg = pkg,
    build_args = c("--no-build-vignettes"),
    args = c("--ignore-vignettes")
  )

}

#' @export
#'
#' @title Package check without building vignettes or examples
#'
#' @description Runs devtools::check() with appropriate arguments to avoid
#' building or checking vignettes or examples.
#' This is a wrapper for:
#'
#' \preformatted{
#' devtools::check(
#'   build_args = c("--no-build-vignettes"),
#'   args = c("--ignore-vignettes", "--no-examples")
#' )
#' }
#'
#' @param pkg passed to \code{devtools::check()}
#'
#' @return No return.

check_faster <- function(
  pkg = "."
) {

  devtools::check(
    pkg = pkg,
    build_args = c("--no-build-vignettes"),
    args = c("--ignore-vignettes", "--no-examples")
  )

}

#' @export
#'
#' @title Package check without building vignettes, examples or tests
#'
#' @description Runs devtools::check() with appropriate arguments to avoid
#' building or checking documentation, vignettes, examples and tests.
#' This is a wrapper for:
#'
#' \preformatted{
#' devtools::check(
#'   build_args = c("--no-build-vignettes", "--no-manual"),
#'   args = c("--ignore-vignettes", "--no-manual",
#'            "--no-examples", "--no-tests")
#' )
#' }
#'
#' @param pkg passed to \code{devtools::check()}
#'
#' @return No return.

check_fastest <- function(
  pkg = "."
) {

  devtools::check(
    pkg = pkg,
    build_args = c("--no-build-vignettes", "--no-manual"),
    args = c("--ignore-vignettes", "--no-manual",
             "--no-tests", "--no-examples")
  )

}

#' @export
#'
#' @title Check and run donttest examples
#'
#' @description Runs devtools::check() with appropriate arguments to run
#' \code{donttest{...}} examples.
#' This is a wrapper for:
#'
#' \preformatted{
#' devtools::check(
#'   args = c("--run-donttest")
#' )
#' }
#'
#' @param pkg passed to \code{devtools::check()}
#'
#' @return No return.

check_slow <- function(
  pkg = "."
) {

  devtools::check(
    pkg = pkg,
    args = c("--run-donttest")
  )

}

#' @export
#'
#' @title Check and run donttest and dontrun examples
#'
#' @description Runs devtools::check() with appropriate arguments to run
#' \code{donttest{...}} and \code{dontrun{..}} examples.
#' This is a wrapper for:
#'
#' \preformatted{
#' devtools::check(
#'   args = c("--run-donttest", "--run-dontrun")
#' )
#' }
#'
#' @param pkg passed to \code{devtools::check()}
#'
#' @return No return.

check_slower <- function(
  pkg = "."
) {

  devtools::check(
    pkg = pkg,
    args = c("--run-donttest", "--run-dontrun")
  )

}

#' @export
#'
#' @title Check and run donttest{} and dontrun{} examples, max testing
#'
#' @description Runs devtools::check() with maximal testing options.
#' This is a wrapper for:
#'
#' \preformatted{
#' devtools::check(
#'   args = c("--run-donttest", "--run-dontrun", "--use-gct")
#' )
#' }
#'
#' @param pkg passed to \code{devtools::check()}
#'
#' @return No return.

check_slowest <- function(
  pkg = "."
) {

  devtools::check(
    pkg = pkg,
    args = c("--run-donttest", "--run-dontrun", "--use-gct")
  )

}
