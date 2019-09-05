#' Stop if an object is NULL
#'
#' @description
#' This is a convenience function for testing if an object is \code{NULL}, and
#' providing a custom error message if it is.
#'
#' @param target Object to test if \code{NULL}.
#' @param msg Optional custom message to display when \code{target} is
#'   \code{NULL}.
#'
#' @return If \code{target} is not \code{NULL}, \code{target} is returned
#'   invisibly.
#'
#' @export
#'
#' @examples
#'
#' # Return input invisibly if not NULL
#' x <- stopIfNull(5, msg = "Custom message")
#' print(x)
#'
#' # This can be useful when building pipelines
#' y <- 1:10
#' y_mean <-
#'   y %>%
#'   stopIfNull() %>%
#'   mean()
#'
#' \dontrun{
#' testVar <- NULL
#' stopIfNull(testVar)
#' stopIfNull(testVar, msg = "This is NULL")
#'
#' # Make a failing pipeline
#' z <- NULL
#' z_mean <-
#'   z %>%
#'   stopIfNull("This has failed.") %>%
#'   mean()
#' }
stopIfNull <- function(
  target,
  msg = NULL
) {

  # Return early if not NULL ---------------------------------------------------

  if ( !is.null(target) )
    return(invisible(target))


  # Build error message --------------------------------------------------------

  if ( is.null(msg) ) {

    msg <- paste0("argument '",
                  deparse(substitute(target)),
                  "' must not be NULL.")

  } else if (!is.character(msg) || length(msg) != 1) {

    stop("msg must be a character string of length one.")

  }


  # Stop -----------------------------------------------------------------------

  stop(msg, call. = FALSE)

}
