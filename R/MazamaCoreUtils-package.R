#' MazamaCoreUtils
#'
#' Utility functions for production R code.
#'
#' MazamaCoreUtils provides a standardized set of utility functions used by
#' MazamaScience packages and R-based operational systems.
#'
#' Functionality includes:
#'
#' \itemize{
#'   \item Python-style logging
#'   \item simple error messaging
#'   \item cache management
#'   \item API key handling
#'   \item date parsing and formatting
#'   \item longitude/latitude validation
#'   \item unique location ID creation
#'   \item source code linting
#' }
#'
#' @keywords internal
"_PACKAGE"

# ----- Internal Package State -------------------------------------------------

MazamaCoreUtilsEnv <- new.env(parent = emptyenv())
MazamaCoreUtilsEnv$dataDir <- NULL
MazamaCoreUtilsEnv$APIKeys <- list()

# ----- API Keys ---------------------------------------------------------------

#' API keys for data services
#'
#' Internal session state used to store API keys for web services.
#'
#' Users can set API keys with [setAPIKey()]. Keys are remembered for the
#' duration of the R session and can be retrieved with [getAPIKey()].
#'
#' This provides a small abstraction layer for dependent packages so that data
#' access functions can test for and retrieve provider-specific API keys with
#' generic code.
#'
#' @name APIKeys
#' @docType data
#' @keywords environment
#'
#' @format
#' A named list of character strings.
#'
#' @seealso
#' [getAPIKey()], [setAPIKey()], [showAPIKeys()]
NULL

#' Show API keys
#'
#' Print all currently set API keys.
#'
#' @return
#' No return value. Called for side effects.
#'
#' @seealso
#' [getAPIKey()], [setAPIKey()]
#'
#' @keywords environment
#' @export
#' @importFrom utils str
showAPIKeys <- function() {
  utils::str(MazamaCoreUtilsEnv$APIKeys)
}

#' Get API key
#'
#' Return the API key associated with a web service provider.
#'
#' If `provider = NULL`, all currently stored API keys are returned.
#'
#' @param provider Web service provider.
#'
#' @return
#' API key string, `NULL`, or a named list of all provider/key pairs.
#'
#' @seealso
#' [APIKeys], [setAPIKey()], [showAPIKeys()]
#'
#' @keywords environment
#' @export
getAPIKey <- function(provider = NULL) {
  if ( is.null(provider) ) {
    return(MazamaCoreUtilsEnv$APIKeys)
  } else {
    return(MazamaCoreUtilsEnv$APIKeys[[provider]])
  }
}

#' Set API key
#'
#' Set the API key associated with a web service provider.
#'
#' API keys are stored in package session state and are remembered only for the
#' duration of the current R session.
#'
#' @param provider Web service provider.
#' @param key API key.
#'
#' @return
#' Invisibly returns the previous value of the API key.
#'
#' @seealso
#' [getAPIKey()], [showAPIKeys()]
#'
#' @keywords environment
#' @export
setAPIKey <- function(provider = NULL, key = NULL) {
  old <- MazamaCoreUtilsEnv$APIKeys[[provider]]
  MazamaCoreUtilsEnv$APIKeys[[provider]] <- key
  return(invisible(old))
}

