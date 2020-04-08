#' @name packageCheck
#' @aliases check_slow check
#'
#' @title Run package checks
#'
#' @param pkg Package location passed to \code{devtools::check()}.
#'
#' @return No return.
#'
#' @description When multiple devlopers are working on a package, it is
#' crucially important that they check their code changes \emph{often}.  After
#' merging changes from multiple developers it is equally important to check the
#' package \emph{thoroughly}.
#'
#' The problem is that frequent checks should be quick or developers won't do
#' them while thorough checks are, by nature, slow.
#'
#' Our solution is to provide shorthand functions that wrap
#' \code{devtools::check()} and pass it a variety of different arguments.
#'
#' @details The table below describes the \code{args} passed:
#'
#' \tabular{rl}{
#' \code{check_slowest()} \tab | \code{args = c("--run-donttest", "--run-dontrun", "--use-gct")}\cr
#' \code{check_slower()} \tab | \code{args = c("--run-donttest", "--run-dontrun")}\cr
#' \code{check_slow()} \tab | \code{args = c("--run-donttest")}\cr
#' \code{check_()} \tab | \code{args = c()}\cr
#' \code{check_fast()} \tab | \code{args = c("--ignore-vignettes")}\cr
#'  \tab | \code{build_args = c("--no-build-vignettes")}\cr
#' \code{check_fastest()} \tab | \code{args = c("--ignore-vignettes", "--no-examples")}\cr
#'  \tab | \code{build_args = c("--no-build-vignettes", "--no-examples")}\cr
#' \code{check_fastest()} \tab | \code{args = c("--ignore-vignettes", "--no-examples", "--no-manual", "--no-tests")}\cr
#'  \tab | \code{build_args = c("--no-build-vignettes", "--no-manual")}\cr
#' }
#'
#'
#' @seealso \code{\link[devtools]{check}}
#'
NULL





#' @rdname packageCheck
#' @export
check <- function(
  pkg = "."
) {

  devtools::check(
    pkg = pkg
  )

}

#' @rdname packageCheck
#' @export
check_fast <- function(
  pkg = "."
) {

  devtools::check(
    pkg = pkg,
    build_args = c("--no-build-vignettes"),
    args = c("--ignore-vignettes")
  )

}

#' @rdname packageCheck
#' @export
check_faster <- function(
  pkg = "."
) {

  devtools::check(
    pkg = pkg,
    build_args = c("--no-build-vignettes"),
    args = c("--ignore-vignettes", "--no-examples")
  )

}

#' @rdname packageCheck
#' @export
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

#' @rdname packageCheck
#' @export
check_slow <- function(
  pkg = "."
) {

  devtools::check(
    pkg = pkg,
    args = c("--run-donttest")
  )

}

#' @rdname packageCheck
#' @export
check_slower <- function(
  pkg = "."
) {

  devtools::check(
    pkg = pkg,
    args = c("--run-donttest", "--run-dontrun")
  )

}

#' @rdname packageCheck
#' @export
check_slowest <- function(
  pkg = "."
) {

  devtools::check(
    pkg = pkg,
    args = c("--run-donttest", "--run-dontrun", "--use-gct")
  )

}
