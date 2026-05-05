#' Stop on try-error
#'
#' Generate a consistent error message from the result of a `try()` block.
#'
#' This function is intended for production code where potentially fragile
#' operations are wrapped in `try(..., silent = TRUE)`. If `result` inherits
#' from `"try-error"`, a cleaned and optionally customized error message is
#' generated and passed to [stop()].
#'
#' If `result` is not a `"try-error"`, the function returns `NULL`.
#'
#' @param result Return value from a `try()` block.
#' @param err_msg Optional custom error message.
#' @param prefix Optional text to prepend to the error message.
#' @param maxLength Maximum allowed error message length before truncation.
#' @param truncatedLength Length of the truncated error message.
#' @param call. Logical indicating whether the call should be included in the
#'   error message. Passed to [stop()].
#'
#' @return
#' Returns `NULL` if `result` is not a `"try-error"`; otherwise stops with an
#' error.
#'
#' @note
#' If logging has been initialized, the final error message is logged with
#' [logger.error()] before calling [stop()].
#'
#' @examples
#' \dontrun{
#' myFunc <- function(x) {
#'   log(x)
#' }
#'
#' result <- try({
#'   myFunc("ten")
#' }, silent = TRUE)
#'
#' stopOnError(result)
#'
#' try({
#'   myFunc("ten")
#' }, silent = TRUE) %>%
#'   stopOnError(err_msg = "Unable to process user input")
#'
#' try({
#'   myFunc("ten")
#' }, silent = TRUE) %>%
#'   stopOnError(
#'     prefix = "USER_INPUT_ERROR",
#'     maxLength = 40,
#'     truncatedLength = 32
#'   )
#' }
#'
#' @name stopOnError
#' @export

stopOnError <- function(
  result,
  err_msg = "",
  prefix = "",
  maxLength = 500,
  truncatedLength = 120,
  call. = FALSE
) {

  if ( "try-error" %in% class(result) ) {

    # Use passed in message or cleaned up version from geterrmessage()
    err_msg <- ifelse(err_msg == "", geterrmessage(), err_msg)

    err_msg <-
      err_msg %>%
      stringr::str_replace("Error : ", "") %>%
      stringr::str_replace("Error: ", "") %>%
      stringr::str_trim()

    if ( prefix != "" )
      err_msg <- paste(prefix, err_msg)

    if ( stringr::str_length(err_msg) > maxLength )
      err_msg <- paste(stringr::str_sub(err_msg, end = truncatedLength), "...")

    if ( logger.isInitialized() )
      logger.error(err_msg)

    stop(err_msg, call. = call.)

  }

}

