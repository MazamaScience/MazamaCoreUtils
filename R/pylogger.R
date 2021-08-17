#' @title Set up python-style logging
#'
#' @description
#' Good logging allows package developers and users to create log files at
#' different levels to track and debug lengthy or complex calculations.
#' "Python-style" logging is intended to suggest that users should set up
#' multiple log files for different log severities so that the \code{errorLog}
#' will contain only log messages at or above the \code{ERROR} level while a
#' \code{debugLog} will contain log messages at the \code{DEBUG} level as well
#' as all higher levels.
#'
#' Python-style log files are set up with \code{logger.setup()}. Logs can be set
#' up for any combination of log levels. Accepting the default \code{NULL}
#' setting for any log file simply means that log file will not be created.
#'
#' Python-style logging requires the use of \code{logger.debug()} style logging
#' statements as seen in the example below.
#'
#' @param traceLog File name or full path where \code{logger.trace()} messages
#'   will be sent.
#' @param debugLog File name or full path where \code{logger.debug()} messages
#'   will be sent.
#' @param infoLog File name or full path where \code{logger.info()} messages
#'   will be sent.
#' @param warnLog File name or full path where \code{logger.warn()} messages
#'   will be sent.
#' @param errorLog File name or full path where \code{logger.error()} messages
#'   will be sent.
#' @param fatalLog File name or full path where \code{logger.fatal()} messages
#'   will be sent.
#' @return No return value.
#'
#' @note All functionality is built on top of the excellent \pkg{futile.logger}
#'   package.
#'
#' @name logger.setup
#'
#' @importFrom futile.logger appender.console appender.file appender.tee
#' @importFrom futile.logger flog.appender flog.logger flog.threshold
#' @importFrom futile.logger flog.debug flog.error flog.fatal flog.info
#' @importFrom futile.logger flog.trace flog.warn
#' @importFrom futile.logger flog.layout
#' @export
#'
#' @examples
#' \dontrun{
#' library(MazamaCoreUtils)
#'
#' # Only save three log files
#' logger.setup(
#'   debugLog = "debug.log",
#'   infoLog = "info.log",
#'   errorLog = "error.log"
#' )
#'
#' # But allow lot statements at all levels within the code
#' logger.trace("trace statement #%d", 1)
#' logger.debug("debug statement")
#' logger.info("info statement %s %s", "with", "arguments")
#' logger.warn("warn statement %s", "about to try something dumb")
#' result <- try(1/"a", silent=TRUE)
#' logger.error("error message: %s", geterrmessage())
#' logger.fatal("fatal statement %s", "THE END")
#' }
#'
#' @seealso \code{\link{logger.trace}} \code{\link{logger.debug}}
#'   \code{\link{logger.info}} \code{\link{logger.warn}}
#'   \code{\link{logger.error}} \code{\link{logger.fatal}}
#'
# Set up logging namespaces
logger.setup <- function(
  traceLog = NULL,
  debugLog = NULL,
  infoLog = NULL,
  warnLog = NULL,
  errorLog = NULL,
  fatalLog = NULL
) {

  if (! "futile.logger" %in% loadedNamespaces()) {
    requireNamespace("futile.logger", quietly = TRUE)
  }

  # By default, the console receives only FATAL messages.
  flog.threshold(FATAL)

  # Set up TRACE logging
  if (is.null(traceLog)) {
    invisible(flog.logger("trace", TRACE, appender.null()))
  } else {
    if (file.exists(traceLog)) result <- file.remove(traceLog)
    invisible(flog.logger("trace", TRACE, appender.file(traceLog)))
  }

  # Set up DEBUG logging
  if (is.null(debugLog)) {
    invisible(flog.logger("debug", DEBUG, appender.null()))
  } else {
    if (file.exists(debugLog)) result <- file.remove(debugLog)
    invisible(flog.logger("debug", DEBUG, appender.file(debugLog)))
  }

  # Set up INFO logging
  if (is.null(infoLog)) {
    invisible(flog.logger("info", INFO, appender.null()))
  } else {
    if (file.exists(infoLog)) result <- file.remove(infoLog)
    invisible(flog.logger("info", INFO, appender.file(infoLog)))
  }

  # Set up WARN logging
  if (is.null(warnLog)) {
    invisible(flog.logger("warn", WARN, appender.null()))
  } else {
    if (file.exists(warnLog)) result <- file.remove(warnLog)
    invisible(flog.logger("warn", WARN, appender.file(warnLog)))
  }

  # Set up ERROR logging
  if (is.null(errorLog)) {
    invisible(flog.logger("error", ERROR, appender.null()))
  } else {
    if (file.exists(errorLog)) result <- file.remove(errorLog)
    invisible(flog.logger("error", ERROR, appender.file(errorLog)))
  }

  # Set up FATAL logging
  if (is.null(fatalLog)) {
    invisible(flog.appender(appender.console(), name = "ROOT"))
  } else {
    if (file.exists(fatalLog)) result <- file.remove(fatalLog)
    invisible(flog.appender(appender.tee(fatalLog)))
  }

}


#' @title Check for initialization of loggers
#'
#' @description
#' Returns \code{TRUE} if logging has been initialized. This allows packages
#' to emit logging statements only if logging has already been set up,
#' potentially avoiding `futile.log` errors.
#'
#' @return \code{TRUE} if logging has already been initialized.
#'
#' @name logger.isInitialized
#' @importFrom futile.logger flog.threshold
#' @export
#'
#' @examples
#' \dontrun{
#' logger.isInitialized()
#' logger.setup()
#' logger.isInitialized()
#' }
#'
#' @seealso \code{\link{logger.setup}}
#' @seealso \code{\link{initializeLogging}}
#'
logger.isInitialized <- function() {
  if ( "futile.logger" %in% loadedNamespaces() ) {
    if (
      futile.logger::flog.logger("trace")$threshold == futile.logger::TRACE &&
      futile.logger::flog.logger("debug")$threshold == futile.logger::DEBUG &&
      futile.logger::flog.logger("info")$threshold == futile.logger::INFO &&
      futile.logger::flog.logger("warn")$threshold == futile.logger::WARN &&
      futile.logger::flog.logger("error")$threshold == futile.logger::ERROR
    ) {
      return(TRUE)
    }
  }
  return(FALSE)
}


#' @title Set console log level
#'
#' @description
#' By default, the logger threshold is set to \code{FATAL} so that the console
#' will typically receive no log messages. By setting the level to one of the
#' other log levels: \code{TRACE, DEBUG, INFO, WARN, ERROR} users can see
#' logging messages while running commands at the command line.
#'
#' @param level Threshold level.
#' @return No return value.
#'
#' @note All functionality is built on top of the excellent \pkg{futile.logger}
#'   package.
#'
#' @name logger.setLevel
#' @importFrom futile.logger flog.threshold
#' @export
#'
#' @examples
#' \dontrun{
#' # Set up console logging only
#' logger.setup()
#' logger.setLevel(DEBUG)
#' }
#'
#' @seealso \code{\link{logger.setup}}
#'
logger.setLevel <- function(level) {
  if ( !logger.isInitialized() ) {
    logger.setup()
  }
  invisible(flog.threshold(level))
}


#' @name logger.trace
#' @export
#' @importFrom futile.logger flog.trace
#' @title Python-style logging statements
#' @param msg Message with format strings applied to additional arguments.
#' @param \dots Additional arguments to be formatted.
#' @return No return value.
#' @description After initializing the level-specific log files with \code{logger.setup(...)},
#' this function will generate \code{TRACE} level log statements.
#' @note All functionality is built on top of the excellent \pkg{futile.logger} package.
#' @examples
#' \dontrun{
#' # Only save three log files
#' logger.setup(
#'   debugLog = "debug.log",
#'   infoLog = "info.log",
#'   errorLog = "error.log"
#' )
#'
#' # But allow log statements at all levels within the code
#' logger.trace("trace statement #%d", 1)
#' logger.debug("debug statement")
#' logger.info("info statement %s %s", "with", "arguments")
#' logger.warn("warn statement %s", "about to try something dumb")
#' result <- try(1/"a", silent=TRUE)
#' logger.error("error message: %s", geterrmessage())
#' logger.fatal("fatal statement %s", "THE END")
#' }
#' @seealso \code{\link{logger.setup}}

# Log at the TRACE level
logger.trace <- function(msg, ...) {
  .stopIfNotInitilized()
  flog.trace(msg, ..., name = "ROOT")
  flog.trace(msg, ..., name = "trace")
}

#' @name logger.debug
#' @export
#' @importFrom futile.logger flog.debug
#' @title Python-style logging statements
#' @param msg Message with format strings applied to additional arguments.
#' @param \dots Additional arguments to be formatted.
#' @return No return value.
#' @description After initializing the level-specific log files with \code{logger.setup(...)},
#' this function will generate \code{DEBUG} level log statements.
#' @note All functionality is built on top of the excellent \pkg{futile.logger} package.
#' @examples
#' \dontrun{
#' # Only save three log files
#' logger.setup(
#'   debugLog = "debug.log",
#'   infoLog = "info.log",
#'   errorLog = "error.log"
#' )
#'
#' # But allow log statements at all levels within the code
#' logger.trace("trace statement #%d", 1)
#' logger.debug("debug statement")
#' logger.info("info statement %s %s", "with", "arguments")
#' logger.warn("warn statement %s", "about to try something dumb")
#' result <- try(1/"a", silent=TRUE)
#' logger.error("error message: %s", geterrmessage())
#' logger.fatal("fatal statement %s", "THE END")
#' }
#' @seealso \code{\link{logger.setup}}

# Log at the DEBUG level
logger.debug <- function(msg, ...) {
  .stopIfNotInitilized()
  flog.debug(msg, ..., name = "ROOT")
  flog.debug(msg, ..., name = "trace")
  flog.debug(msg, ..., name = "debug")
}

#' @name logger.info
#' @export
#' @importFrom futile.logger flog.debug
#' @title Python-style logging statements
#' @param msg Message with format strings applied to additional arguments.
#' @param \dots Additional arguments to be formatted.
#' @return No return value.
#' @description After initializing the level-specific log files with \code{logger.setup(...)},
#' this function will generate \code{INFO} level log statements.
#' @note All functionality is built on top of the excellent \pkg{futile.logger} package.
#' @examples
#' \dontrun{
#' # Only save three log files
#' logger.setup(
#'   debugLog = "debug.log",
#'   infoLog = "info.log",
#'   errorLog = "error.log"
#' )
#'
#' # But allow log statements at all levels within the code
#' logger.trace("trace statement #%d", 1)
#' logger.debug("debug statement")
#' logger.info("info statement %s %s", "with", "arguments")
#' logger.warn("warn statement %s", "about to try something dumb")
#' result <- try(1/"a", silent=TRUE)
#' logger.error("error message: %s", geterrmessage())
#' logger.fatal("fatal statement %s", "THE END")
#' }
#' @seealso \code{\link{logger.setup}}

# Log at the INFO level
logger.info <- function(msg, ...) {
  .stopIfNotInitilized()
  flog.info(msg, ..., name = "ROOT")
  flog.info(msg, ..., name = "trace")
  flog.info(msg, ..., name = "debug")
  flog.info(msg, ..., name = "info")
}

#' @name logger.warn
#' @export
#' @importFrom futile.logger flog.warn
#' @title Python-style logging statements
#' @param msg Message with format strings applied to additional arguments.
#' @param \dots Additional arguments to be formatted.
#' @return No return value.
#' @description After initializing the level-specific log files with \code{logger.setup(...)},
#' this function will generate \code{WARN} level log statements.
#' @note All functionality is built on top of the excellent \pkg{futile.logger} package.
#' @examples
#' \dontrun{
#' # Only save three log files
#' logger.setup(
#'   debugLog = "debug.log",
#'   infoLog = "info.log",
#'   errorLog = "error.log"
#' )
#'
#' # But allow log statements at all levels within the code
#' logger.trace("trace statement #%d", 1)
#' logger.debug("debug statement")
#' logger.info("info statement %s %s", "with", "arguments")
#' logger.warn("warn statement %s", "about to try something dumb")
#' result <- try(1/"a", silent=TRUE)
#' logger.error("error message: %s", geterrmessage())
#' logger.fatal("fatal statement %s", "THE END")
#' }
#' @seealso \code{\link{logger.setup}}

# Log at the WARN level
logger.warn <- function(msg, ...) {
  .stopIfNotInitilized()
  flog.warn(msg, ..., name = "ROOT")
  flog.warn(msg, ..., name = "trace")
  flog.warn(msg, ..., name = "debug")
  flog.warn(msg, ..., name = "info")
  flog.warn(msg, ..., name = "warn")
}

#' @name logger.error
#' @export
#' @importFrom futile.logger flog.error
#' @title Python-style logging statements
#' @param msg Message with format strings applied to additional arguments.
#' @param \dots Additional arguments to be formatted.
#' @return No return value.
#' @description After initializing the level-specific log files with \code{logger.setup(...)},
#' this function will generate \code{ERROR} level log statements.
#' @note All functionality is built on top of the excellent \pkg{futile.logger} package.
#' @examples
#' \dontrun{
#' # Only save three log files
#' logger.setup(
#'   debugLog = "debug.log",
#'   infoLog = "info.log",
#'   errorLog = "error.log"
#' )
#'
#' # But allow log statements at all levels within the code
#' logger.trace("trace statement #%d", 1)
#' logger.debug("debug statement")
#' logger.info("info statement %s %s", "with", "arguments")
#' logger.warn("warn statement %s", "about to try something dumb")
#' result <- try(1/"a", silent=TRUE)
#' logger.error("error message: %s", geterrmessage())
#' logger.fatal("fatal statement %s", "THE END")
#' }
#' @seealso \code{\link{logger.setup}}

# Log at the ERROR level
logger.error <- function(msg, ...) {
  .stopIfNotInitilized()
  flog.error(msg, ..., name = "ROOT")
  flog.error(msg, ..., name = "trace")
  flog.error(msg, ..., name = "debug")
  flog.error(msg, ..., name = "info")
  flog.error(msg, ..., name = "warn")
  flog.error(msg, ..., name = "error")
}

#' @name logger.fatal
#' @export
#' @importFrom futile.logger flog.fatal
#' @title Python-style logging statements
#' @param msg Message with format strings applied to additional arguments.
#' @param \dots Additional arguments to be formatted.
#' @return No return value.
#' @description After initializing the level-specific log files with \code{logger.setup(...)},
#' this function will generate \code{FATAL} level log statements.
#' @note All functionality is built on top of the excellent \pkg{futile.logger} package.
#' @examples
#' \dontrun{
#' # Only save three log files
#' logger.setup(
#'   debugLog = "debug.log",
#'   infoLog = "info.log",
#'   errorLog = "error.log"
#' )
#'
#' # But allow log statements at all levels within the code
#' logger.trace("trace statement #%d", 1)
#' logger.debug("debug statement")
#' logger.info("info statement %s %s", "with", "arguments")
#' logger.warn("warn statement %s", "about to try something dumb")
#' result <- try(1/"a", silent=TRUE)
#' logger.error("error message: %s", geterrmessage())
#' logger.fatal("fatal statement %s", "THE END")
#' }
#' @seealso \code{\link{logger.setup}}

# Log at the fatal level
logger.fatal <- function(msg, ...) {
  .stopIfNotInitilized()
  flog.fatal(msg, ..., name = "ROOT")
  flog.fatal(msg, ..., name = "trace")
  flog.fatal(msg, ..., name = "debug")
  flog.fatal(msg, ..., name = "info")
  flog.fatal(msg, ..., name = "warn")
  flog.fatal(msg, ..., name = "error")
}

# Utility functions ------------------------------------------------------------

# This function is missing from futile.logger
appender.null <- function() {
  function(line) invisible(NULL)
}

# Quick test if futile.logger namespace is loaded
.stopIfNotInitilized <- function() {
  if (! "futile.logger" %in% loadedNamespaces()) {
    stop(
      "You must initialize with 'logger.setup()' before issuing logger statements.",
      call. = FALSE
    )
  }
}


# Constants --------------------------------------------------------------------

# Verbatim values from constants
#' @docType data
#' @name logLevels
#' @aliases FATAL ERROR WARN INFO DEBUG TRACE
#' @title Log levels
#' @description Log levels matching those found in \pkg{futile.logger}.
#' Available levels include:
#'
#' \code{FATAL ERROR WARN INFO DEBUG TRACE}
#' @export
FATAL <- 1L
names(FATAL) <- "FATAL"
#' @export
ERROR <- 2L
names(ERROR) <- "ERROR"
#' @export
WARN <- 4L
names(WARN) <- "WARN"
#' @export
INFO <- 6L
names(INFO) <- "INFO"
#' @export
DEBUG <- 8L
names(DEBUG) <- "DEBUG"
#' @export
TRACE <- 9L
names(TRACE) <- "TRACE"
