#' Run package checks
#'
#' Convenience wrappers around [devtools::check()] for package checking at
#' different levels of thoroughness.
#'
#' These functions make it easy to run quick checks during active development
#' and more thorough checks before merging or releasing package changes.
#'
#' @param pkg Package location passed to [devtools::check()].
#'
#' @return
#' Invisibly returns the result from [devtools::check()].
#'
#' @details
#' The functions are ordered from most thorough to fastest:
#'
#' \describe{
#'   \item{`check_slowest()`}{
#'     Builds the manual, runs `donttest` and `dontrun` examples, and
#'     uses `--use-gct`.
#'   }
#'   \item{`check_slower()`}{
#'     Builds the manual and runs `donttest` and `dontrun` examples.
#'   }
#'   \item{`check_slow()`}{
#'     Builds the manual and runs `donttest` examples.
#'   }
#'   \item{`check()`}{
#'     Standard development check without building the manual or running
#'     `donttest` examples.
#'   }
#'   \item{`check_fast()`}{
#'     Skips vignette building and ignores vignettes during checking.
#'   }
#'   \item{`check_faster()`}{
#'     Skips vignette building, ignores vignettes, and skips examples.
#'   }
#'   \item{`check_fastest()`}{
#'     Skips vignette building, ignores vignettes, skips examples, and skips
#'     tests.
#'   }
#' }
#'
#' @seealso
#' [devtools::check()]
#'
#' @name packageCheck
#' @aliases check_slow check
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
