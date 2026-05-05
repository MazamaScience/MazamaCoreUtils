#' Stop if an object is `NULL`
#'
#' Convenience function for validating that an object is not `NULL`.
#'
#' If `target` is not `NULL`, it is returned invisibly. If `target` is
#' `NULL`, the function stops with either a default or user-supplied
#' error message.
#'
#' This function is especially useful for validating required function
#' arguments or for guarding intermediate results in pipelines.
#'
#' @param target Object to test.
#' @param msg Optional error message to display if `target` is `NULL`.
#'   Must be a character string of length one.
#'
#' @return
#' Invisibly returns `target` when it is not `NULL`.
#'
#' @examples
#' # Return input invisibly if not NULL
#' x <- stopIfNull(5)
#' print(x)
#'
#' # Useful in pipelines
#' y <- 1:10
#' y_mean <-
#'   y %>%
#'   stopIfNull() %>%
#'   mean()
#'
#' \dontrun{
#' # Trigger the default error message
#' testVar <- NULL
#' stopIfNull(testVar)
#'
#' # Trigger a custom error message
#' stopIfNull(testVar, msg = "This is NULL")
#'
#' # Make a failing pipeline
#' z <- NULL
#' z_mean <-
#'   z %>%
#'   stopIfNull("This has failed.") %>%
#'   mean()
#' }
#'
#' @export
stopIfNull <- function(
    target,
    msg = NULL
) {

  # Return early if not NULL ---------------------------------------------------

  if ( !is.null(target) )
    return(invisible(target))


  # Build error message --------------------------------------------------------

  if ( is.null(msg) ) {

    msg <- paste0(
      "argument '",
      deparse(substitute(target)),
      "' must not be NULL."
    )

  } else if ( !is.character(msg) || length(msg) != 1 ) {

    stop("'msg' must be a character string of length one")

  }


  # Stop -----------------------------------------------------------------------

  stop(msg)

}
