#' @title Set a variable to a default value if it is NULL
#'
#' @description
#' This function attempts to set a default value for a given target object. If
#' the object is \code{NULL}, a default value is returned.
#'
#' When the target object is not \code{NULL}, this function will try and coerce
#' it to match the type if the default (given by \code{\link[base]{typeof}}).
#' This is useful in situations where we are looking to parse the input as well,
#' such at looking at elements of an API call string and wanting to set the
#' character numbers as actual numeric types.
#'
#' Not all coercions are possible, however, and if the function encounters one
#' of these (ex: \code{setIfNull("foo", 5)}) the function will fail.
#'
#' @param target Variable to test if \code{NULL}
#' @param default Variable to return if \code{target} is \code{NULL}
#'
#' @return If \code{target} is not \code{NULL}, then \code{target} coerced to
#'   the type of \code{default}. Otherwise, \code{default} is returned.
#'
#' @section Possible Coercions:
#' This function checks the type of the target and default as given by
#' \code{\link[base]{typeof}}. Specifically, it accounts for the types:
#'
#' \itemize{
#'   \item \code{character}
#'   \item \code{integer}
#'   \item \code{double}
#'   \item \code{complex}
#'   \item \code{logical}
#'   \item \code{list}
#' }
#'
#' \emph{R} tries to intelligently coerce types, but some coercions from one
#' type to another won't always be possible. Everything can be turned into a
#' character, but only some character objects can become numeric ("7" can,
#' while "hello" cannot). Some other coercions work, but you will lose
#' information in the process. For example, the \emph{double} 5.7 can be coerced
#' into an \emph{integer}, but the decimal portion will be dropped with no
#' rounding. It is important to realize that while it is possible to move
#' between most types, the results are not always meaningful.
#'
#' @export
#'
#' @examples
#' setIfNull(NULL, "foo")
#' setIfNull(10, 0)
#' setIfNull("15", 0)
#'
#'
#' # This function can be useful for adding elements to a list
#' testList <- ("a" = 1, "b" = "baz", "c" = "4")
#'
#' testList$a <- setIfNull(testList$a, 0)
#' testList$b <- setIfNull(testList$c, 0)
#' testList$d <- setIfNull(testList$d, 6)
#'
#'
#' # Be careful about unintended results
#' setIfNull("T", FALSE) # This returns `TRUE`
#' setIfNull(12.8, 5L)   # This returns the integer 12
#'
#'
#' \dontrun{
#' # Not all coercions are possible
#' setIfNull("bar", 5)
#' setIfNull("5i", 0+0i)
#' setIfNull("t", FALSE)
#' }
setIfNull <- function(target, default) {

  # Validate parameters --------------------------------------------------------

  ## NOTE:
  #  With no default arguments specified, the function will fail if any argument
  #  is missing.


  # Set default and possibly convert type --------------------------------------

  if (is.null(target)) {

    result <- default

  } else {

    # Set up possible coercions
    if (is.character(default)) {
      conversionFunc <- function(x) as.character(x)
    } else if (is.integer(default)) {
      conversionFunc <- function(x) as.integer(x)
    } else if (is.double(default)) {
      conversionFunc <- function(x) as.double(x)
    } else if (is.complex(default)) {
      conversionFunc <- function(x) as.complex(x)
    } else if (is.logical(default)) {
      conversionFunc <- function(x) as.logical(x)
    } else if (is.list(default)) {
      conversionFunc <- function(x) as.list(x)
    } else {
      stop("argument 'default' is not of a supported type.")
    }

    # Suppress warning about NAs when unable to coerce types
    result <- suppressWarnings(conversionFunc(target))

  }


  # Test conversion result -----------------------------------------------------


  # If the target can't be coerced to the type of the defult, NA is produced.

  if (is.na(result))
    stop(paste0(
      "Could not convert target ", typeof(target), " `", target, "` ",
      "to output ", typeof(default), " `", default, "`."
    ))


  # Return result --------------------------------------------------------------

  return(result)

}
