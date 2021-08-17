#' @name stopOnError
#' @export
#' @title Error message translator
#' @param result Return from a \code{try()} block.
#' @param err_msg Custom error message.
#' @return Issues a \code{stop()} with an appropriate error message.
#' @description When writing R code to be used in production systems that work
#' with user supplied input, it is important to enclose chunks of code inside
#' of a \code{try()} block. It is equally important to generate error log
#' messages that can be found and understood during an autopsy when something
#' fails
#'
#' At Mazama Science we have our own internal standard for how to do error
#' handling in a manner that allows us to quickly navigate to the source of
#' errors in a production system.
#'
#' The example section contains a snippet showing how we use this function.
#' @examples
#' \dontrun{
#' library(MazamaCoreUtils)
#'
#' logger.setup()
#'
#' # Arbitrarily deep in the stack we might have:
#' myFunc <- function(x) {
#'   a <- log(x)
#' }
#'
#' userInput <- 10
#' result <- try({
#'   myFunc(x=userInput)
#' }, silent=TRUE)
#' stopOnError(result)
#'
#' userInput <- "ten"
#' result <- try({
#'   myFunc(x=userInput)
#' }, silent=TRUE)
#' stopOnError(result)
#'
#' result <- try({
#'   myFunc(x=userInput)
#' }, silent=TRUE)
#' stopOnError(result, "Unable to process user input")
#'
#' }

stopOnError <- function(
  result,
  err_msg = ""
) {

  if ( "try-error" %in% class(result) ) {

    # Use passed in message or cleaned up version from geterrmessage()
    err_msg <- ifelse(err_msg == "", geterrmessage(), err_msg)
    err_msg <- stringr::str_trim(err_msg)
    logger.error(err_msg)

    # NOTE:  You can copy this function into your top level app code
    # NOTE:  and add custom error messages as in the comment lines below.

    # # TODO:  Convert opaque R error messages to something more friendly
    # if ( stringr::str_detect(err_msg, "HARD TO UNDERSTAND ERROR MESSAGE") ) {
    #   stop("Simple error message.", call. = FALSE)
    # } else {
    #   stop(stringr::str_replace(err_msg, "Error : ", ""), call. = FALSE)
    # }

    # Remove initial error string from error messages percolating up
    # from down the stack with call. = FALSE.
    stop(stringr::str_replace(err_msg, "Error : ", ""), call. = FALSE)

  }

}
