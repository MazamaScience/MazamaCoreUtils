#' @name stopOnError
#' @export
#' @title Error message generator
#' @param result Return from a \code{try()} block.
#' @param err_msg Custom error message.
#' @param prefix Text string to add in front of the error message.
#' @param maxLength Maximum length of an error message. Error messages
#' beyond this limit will be truncated.
#' @param truncatedLength Length of the output error message.
#' @param call. Logical indicating whether the call should become part of the error message.
#'
#' @return Issues a \code{stop()} with an appropriate error message.
#'
#' @description When writing R code for use in production systems, it is
#' important to enclose chunks of code inside of \code{try()} blocks. This is
#' especially important when processing user input or data obtained from web
#' services which may fail for a variety of reasons. If any problems arise
#' within a \code{try()} block, it is important to generate informative and
#' consistent error messages.
#'
#' Over the years, we have developed our own standard protocol for error handling
#' that is easy to understand, easy to implement, and allows for consistent
#' generation of error messages. To goal is to make it easy for developers to test
#' sections of code that might fail and to create more uniform, more informative
#' error messages than those that might come from deep within the \R execution stack.
#'
#' In addition to the generation of custom error messages, use of \code{prefix}
#' allows for the creation of classes of errors that can be detected and handled
#' appropriately as errors propagate to other functions.
#'
#' @note If logging has been initialized, the customized/modified error message
#' will be logged with \code{logger.error(err_msg)} before issuing
#' \code{stop(err_msg)}.
#'
#' The following examples show how to use this function:
#'
#' \preformatted{
#' library(MazamaCoreUtils)
#'
#'
#' # Arbitrarily deep in the stack we might have:
#'
#' myFunc <- function(x) {
#'   a <- log(x)
#' }
#'
#'
#' # Simple usage
#'
#' userInput <- 10
#' result <- try({
#'   myFunc(x = userInput)
#' }, silent = TRUE)
#' stopOnError(result)
#'
#' userInput <- "ten"
#' result <- try({
#'   myFunc(x = userInput)
#' }, silent = TRUE)
#' stopOnError(result)
#'
#'
#' # More concise code with the '\%>\%' operator
#'
#' try({
#'   myFunc(x = userInput)
#' }, silent = TRUE) \%>\%
#' stopOnError(result, err_msg = "Unable to process user input")
#'
#' try({
#'   myFunc(x = userInput)
#' }, silent = TRUE) \%>\%
#' stopOnError(result, prefix = "USER_INPUT_ERROR")
#'
#'
#' # Truncating error message length
#'
#' try({
#'   myFunc(x = userInput)
#' }, silent = TRUE) \%>\%
#' stopOnError(
#'   result,
#'   prefix = "USER_INPUT_ERROR",
#'   maxLength = 40,
#'   truncatedLength = 32
#' )
#'
#' }

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

