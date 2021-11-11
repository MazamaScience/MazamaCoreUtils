#' @docType package
#' @name MazamaCoreUtils
#' @title Utility Functions for Production R Code
#' @description The MazamaCoreUtils package was created by MazamaScience to
#' regularize our work building R-based web services.
#'
#' The main goal of this package is to create an internally standardized set of
#' functions that we can use in various systems that are being run
#' operationally. Areas of functionality supported by this package include:
#'
#' \itemize{
#' \item{ python style logging }
#' \item{ simple error messaging }
#' \item{ cache management }
#' \item{ date parsing }
#' \item{ source code linting }
#' }
NULL


# ----- Internal Package State -------------------------------------------------

MazamaCoreUtilsEnv <- new.env(parent = emptyenv())
MazamaCoreUtilsEnv$dataDir <- NULL
MazamaCoreUtilsEnv$APIKeys <- list()

# ----- API Keys ---------------------------------------------------------------

#' @docType data
#' @keywords environment
#' @name APIKeys
#' @title API keys for data services.
#' @format List of character strings.
#' @description This package maintains an internal set of API keys which
#' users can set using \code{setAPIKey()}. These keys will be remembered for
#' the duration of an R session. This functionality provides an abstraction
#' layer in dependent packages so that data access functions can test for and
#' access specific API keys with generic code.
#' @seealso \link{getAPIKey}
#' @seealso \link{setAPIKey}
#' @seealso \link{showAPIKeys}
NULL

#' @keywords environment
#' @export
#' @importFrom utils str
#' @title Show API keys
#' @description Returns a list of all currently set API keys.
#' @return List of provider:key pairs.
#' @seealso \link{getAPIKey}
#' @seealso \link{setAPIKey}

showAPIKeys <- function() {
  utils::str(MazamaCoreUtilsEnv$APIKeys)
}

#' @keywords environment
#' @export
#' @title Get API key
#' @param provider Web service provider.
#' @description Returns the API key associated with a web service.
#' If \code{provider == NULL} a list is returned containing all recognized
#' API keys.
#' @return API key string or a list of provider:key pairs.
#' @seealso \link{APIKeys}
#' @seealso \link{setAPIKey}
#' @seealso \link{showAPIKeys}

getAPIKey <- function(provider = NULL) {
  if ( is.null(provider) ) {
    return(MazamaCoreUtilsEnv$APIKeys)
  } else {
    return(MazamaCoreUtilsEnv$APIKeys[[provider]])
  }
}

#' @keywords environment
#' @export
#' @title Set APIKey
#' @param provider Web service provider.
#' @param key API key.
#' @description Sets the API key associated with a web service.
#' @return Silently returns previous value of the API key.
#' @seealso \link{getAPIKey}
#' @seealso \link{showAPIKeys}

setAPIKey <- function(provider = NULL, key = NULL) {
  old <- MazamaCoreUtilsEnv$APIKeys[[provider]]
  MazamaCoreUtilsEnv$APIKeys[[provider]] <- key
  return(invisible(old))
}
