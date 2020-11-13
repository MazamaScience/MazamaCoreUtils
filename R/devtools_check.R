#' @name packageCheck
#' @aliases check_slow check
#'
#' @title Run package checks
#'
#' @param pkg Package location passed to \code{devtools::check()}.
#'
#' @return No return.
#'
#' @description When multiple developers are working on a package, it is
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
#' @details The table below describes the \code{args} passed to
#' \code{devtools::check()}:
#'
#' \tabular{rl}{
#' \code{check_slowest()} \tab | \code{manual = TRUE, run_dont_test = TRUE} \cr
#' \tab | \code{args = c("--run-dontrun", "--use-gct")}\cr
#' \code{check_slower()} \tab | \code{manual = TRUE, run_dont_test = TRUE} \cr
#' \tab | \code{args = c("--run-dontrun")}\cr
#' \code{check_slow()} \tab | \code{manual = TRUE, run_dont_test = TRUE}\cr
#' \tab | \code{args = c()}\cr
#' \code{check()} \tab | \code{manual = FALSE, run_dont_test = FALSE} \cr
#' \tab | \code{args = c()}\cr
#' \code{check_fast()} \tab | \code{manual = FALSE, run_dont_test = FALSE} \cr
#' \tab | \code{build_args = c("--no-build-vignettes")}\cr
#' \tab | \code{args = c("--ignore-vignettes")}\cr
#' \code{check_faster()} \tab | \code{manual = FALSE, run_dont_test = FALSE} \cr
#' \tab | \code{build_args = c("--no-build-vignettes")}\cr
#' \tab | \code{args = c("--ignore-vignettes", "--no-examples")}\cr
#' \code{check_fastest()} \tab | \code{manual = FALSE, run_dont_test = FALSE} \cr
#' \tab | \code{build_args = c("--no-build-vignettes")}\cr
#' \tab | \code{args = c("--ignore-vignettes", "--no-examples", "--no-tests")}\cr
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
    pkg = pkg,
    manual = FALSE,
    run_dont_test = FALSE
  )

}

#' @rdname packageCheck
#' @export
check_fast <- function(
  pkg = "."
) {

  devtools::check(
    pkg = pkg,
    manual = FALSE,
    run_dont_test = FALSE,
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
    manual = FALSE,
    run_dont_test = FALSE,
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
    manual = FALSE,
    run_dont_test = FALSE,
    build_args = c("--no-build-vignettes"),
    args = c("--ignore-vignettes", "--no-tests",
             "--no-examples")
  )

}

#' @rdname packageCheck
#' @export
check_slow <- function(
  pkg = "."
) {

  devtools::check(
    pkg = pkg,
    manual = TRUE,
    run_dont_test = TRUE,
  )

}

#' @rdname packageCheck
#' @export
check_slower <- function(
  pkg = "."
) {

  devtools::check(
    pkg = pkg,
    manual = TRUE,
    run_dont_test = TRUE,
    args = c("--run-dontrun")
  )

}

#' @rdname packageCheck
#' @export
check_slowest <- function(
  pkg = "."
) {

  devtools::check(
    pkg = pkg,
    manual = TRUE,
    run_dont_test = TRUE,
    args = c("--run-dontrun", "--use-gct")
  )

}
