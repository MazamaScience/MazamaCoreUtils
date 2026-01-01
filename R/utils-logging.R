# utils-logging.R
#
# Refactor note (2025-12-31):
#   This file now uses the {logger} package under the hood while retaining the
#   MazamaCoreUtils public API:
#     - logger.setup()
#     - logger.isInitialized()
#     - logger.setLevel()
#     - logger.trace()/debug()/info()/warn()/error()/fatal()
#
#   The exported log level constants (FATAL/ERROR/WARN/INFO/DEBUG/TRACE) are
#   retained for backwards compatibility.

# ------------------------------------------------------------------------------
# Internal constants + helpers
# ------------------------------------------------------------------------------

.MAZAMA_LOG_NAMESPACE <- "MazamaCoreUtils"

# This function is conceptually "appender.null()" from futile.logger.
# Used when a given log file is NULL (disabled).
appender.null <- function() {
  function(lines) invisible(NULL)
}

# Map MazamaCoreUtils levels (and/or logger levels) to logger package constants.
.logger_map_level <- function(level) {
  # Allow passing the MazamaCoreUtils exported constants, which have names().
  if (is.numeric(level) && length(level) == 1L && !is.null(names(level))) {
    lvl_name <- names(level)[1]
  } else if (is.character(level) && length(level) == 1L) {
    lvl_name <- level
  } else if (is.numeric(level) && length(level) == 1L) {
    # Best effort: if user passes a raw integer, assume it's already a logger level.
    return(level)
  } else {
    stop("Invalid log level. Use one of: TRACE, DEBUG, INFO, WARN, ERROR, FATAL.", call. = FALSE)
  }

  lvl_name <- toupper(lvl_name)

  if (!requireNamespace("logger", quietly = TRUE)) {
    stop("Package 'logger' must be installed to use MazamaCoreUtils logging.", call. = FALSE)
  }

  # logger has OFF, FATAL, ERROR, WARN, SUCCESS, INFO, DEBUG, TRACE
  if (!exists(lvl_name, envir = asNamespace("logger"), inherits = FALSE)) {
    stop(sprintf("Unknown log level '%s'.", lvl_name), call. = FALSE)
  }

  get(lvl_name, envir = asNamespace("logger"), inherits = FALSE)
}

# Remove all existing logger indices in our namespace to make setup idempotent.
#
# IMPORTANT:
#   The {logger} package expects at least one config/index in a namespace.
#   Deleting down to zero can trigger internal "integerOneIndex" errors when
#   setting layout/threshold/appender later.
#
# Strategy:
#   - If indices exist, delete indices 2..n, then "re-initialize" index 1 by
#     setting its appender/formatter/layout/threshold.
#   - If no indices exist, we will create index 1 by calling log_appender()
#     first in logger.setup().
.logger_reset_namespace <- function() {
  if (!requireNamespace("logger", quietly = TRUE)) {
    stop("Package 'logger' must be installed to use MazamaCoreUtils logging.", call. = FALSE)
  }

  ns <- .MAZAMA_LOG_NAMESPACE
  n <- logger::log_indices(namespace = ns)

  if (is.na(n) || n <= 1L) {
    return(invisible(NULL))
  }

  # Delete in reverse order (but keep index 1).
  for (idx in seq.int(from = n, to = 2L, by = -1L)) {
    logger::delete_logger_index(namespace = ns, index = idx)
  }

  invisible(NULL)
}

# Quick test if logging has been initialized (MazamaCoreUtils view of the world)
.stopIfNotInitilized <- function() {
  if (!isTRUE(getOption("MazamaCoreUtils.logger.initialized", FALSE))) {
    stop(
      "You must initialize with 'logger.setup()' before issuing logger statements.",
      call. = FALSE
    )
  }
}

# ------------------------------------------------------------------------------
# Internal layout
# ------------------------------------------------------------------------------

.mazama_layout <- function(level, msg, namespace, index, ...) {

  # --------------------------------------------------------------------------
  # Determine level name
  # --------------------------------------------------------------------------

  # Preferred: loglevel objects carry the name as an attribute
  lvl <- attr(level, "level", exact = TRUE)

  # Fallback: bare integer (defensive, but rare)
  if (is.null(lvl)) {
    lvl <- switch(
      as.character(level),
      "0"   = "OFF",
      "100" = "FATAL",
      "200" = "ERROR",
      "300" = "WARN",
      "350" = "SUCCESS",
      "400" = "INFO",
      "500" = "DEBUG",
      "600" = "TRACE",
      as.character(level)
    )
  }

  # --------------------------------------------------------------------------
  # Format fields
  # --------------------------------------------------------------------------

  # Pad level to fixed width so timestamps align
  lvl <- sprintf("%-5s", toupper(lvl))

  # Timestamp in UTC with timezone
  ts <- format(
    as.POSIXct(Sys.time(), tz = "UTC"),
    "%Y-%m-%d %H:%M:%S %Z"
  )

  sprintf("%s [%s] %s", lvl, ts, msg)
}

# ------------------------------------------------------------------------------
# Public API
# ------------------------------------------------------------------------------

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
#' @note All functionality is built on top of the excellent \pkg{logger}
#'   package.
#'
#' @name logger.setup
#'
#' @importFrom logger log_appender log_layout log_threshold
#' @importFrom logger log_formatter
#' @importFrom logger appender_console appender_file appender_tee appender_void
#' @importFrom logger layout_simple
#' @importFrom logger formatter_sprintf
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
#' # But allow log statements at all levels within the code
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
logger.setup <- function(
    traceLog = NULL,
    debugLog = NULL,
    infoLog  = NULL,
    warnLog  = NULL,
    errorLog = NULL,
    fatalLog = NULL
) {
  if (!requireNamespace("logger", quietly = TRUE)) {
    stop("Package 'logger' must be installed to use MazamaCoreUtils logging.", call. = FALSE)
  }

  ns <- .MAZAMA_LOG_NAMESPACE

  # Make setup idempotent (but do not delete index 1 down to zero indices).
  .logger_reset_namespace()

  # Ensure index 1 exists by setting an appender FIRST.
  # (logger will create the config slot when we set an appender.)
  if (is.null(fatalLog)) {
    # IMPORTANT: appender_console is a function; do NOT call it.
    logger::log_appender(logger::appender_console, namespace = ns, index = 1L)
  } else {
    if (file.exists(fatalLog)) file.remove(fatalLog)
    # appender_tee() is a factory (needs file), so DO call it.
    logger::log_appender(logger::appender_tee(fatalLog), namespace = ns, index = 1L)
  }

  # Use formatter_sprintf so existing "printf-style" calls continue to work.
  logger::log_formatter(logger::formatter_sprintf, namespace = ns, index = 1L)

  # layout_simple is a layout function; do NOT call it.
  logger::log_layout(.mazama_layout, namespace = ns, index = 1L)

  # By default, the console receives only FATAL messages.
  logger::log_threshold(.logger_map_level(FATAL), namespace = ns, index = 1L)

  # Helper for per-level file appenders (indices 2..6).
  # NOTE: Must set appender first (creates index config slot), then formatter/layout/threshold.
  set_file_logger <- function(index, level_const, path) {

    if (is.null(path)) {
      # Prefer built-in void appender; consistent with {logger}.
      logger::log_appender(logger::appender_void, namespace = ns, index = index)
    } else {
      if (file.exists(path)) file.remove(path)
      logger::log_appender(logger::appender_file(path), namespace = ns, index = index)
    }

    logger::log_formatter(logger::formatter_sprintf, namespace = ns, index = index)
    logger::log_layout(.mazama_layout, namespace = ns, index = index)
    logger::log_threshold(.logger_map_level(level_const), namespace = ns, index = index)

    invisible(NULL)
  }

  # Indices are fixed so downstream behavior is stable and easy to reason about.
  set_file_logger(index = 2L, level_const = TRACE, path = traceLog)
  set_file_logger(index = 3L, level_const = DEBUG, path = debugLog)
  set_file_logger(index = 4L, level_const = INFO,  path = infoLog)
  set_file_logger(index = 5L, level_const = WARN,  path = warnLog)
  set_file_logger(index = 6L, level_const = ERROR, path = errorLog)

  options("MazamaCoreUtils.logger.initialized" = TRUE)

  invisible(NULL)
}

#' @title Check for initialization of loggers
#'
#' @description
#' Returns \code{TRUE} if logging has been initialized. This allows packages
#' to emit logging statements only if logging has already been set up.
#'
#' @return \code{TRUE} if logging has already been initialized.
#'
#' @name logger.isInitialized
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
logger.isInitialized <- function() {
  isTRUE(getOption("MazamaCoreUtils.logger.initialized", FALSE))
}

#' @title Set console log level
#'
#' @description
#' By default, the console logger threshold is set to \code{FATAL} so that the console
#' will typically receive no log messages. By setting the level to one of the
#' other log levels: \code{TRACE, DEBUG, INFO, WARN, ERROR} users can see
#' logging messages while running commands at the command line.
#'
#' @param level Threshold level.
#' @return No return value.
#'
#' @note All functionality is built on top of the excellent \pkg{logger}
#'   package.
#'
#' @name logger.setLevel
#' @importFrom logger log_threshold
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
logger.setLevel <- function(level) {
  if (!logger.isInitialized()) {
    logger.setup()
  }

  lvl <- .logger_map_level(level)

  # Console is always index 1 in our namespace.
  invisible(logger::log_threshold(lvl, namespace = .MAZAMA_LOG_NAMESPACE, index = 1L))
}

# ------------------------------------------------------------------------------
# Logging functions (retain MazamaCoreUtils API)
# ------------------------------------------------------------------------------

#' @name logger.trace
#' @export
#' @importFrom logger log_trace
#' @title Python-style logging statements
#' @param msg Message with format strings applied to additional arguments.
#' @param \dots Additional arguments to be formatted.
#' @return No return value.
#' @description After initializing the level-specific log files with \code{logger.setup(...)},
#' this function will generate \code{TRACE} level log statements.
#' @note All functionality is built on top of the excellent \pkg{logger} package.
#' @seealso \code{\link{logger.setup}}
logger.trace <- function(msg, ...) {
  .stopIfNotInitilized()
  logger::log_trace(msg, ..., namespace = .MAZAMA_LOG_NAMESPACE)
}

#' @name logger.debug
#' @export
#' @importFrom logger log_debug
#' @title Python-style logging statements
#' @param msg Message with format strings applied to additional arguments.
#' @param \dots Additional arguments to be formatted.
#' @return No return value.
#' @description After initializing the level-specific log files with \code{logger.setup(...)},
#' this function will generate \code{DEBUG} level log statements.
#' @note All functionality is built on top of the excellent \pkg{logger} package.
#' @seealso \code{\link{logger.setup}}
logger.debug <- function(msg, ...) {
  .stopIfNotInitilized()
  logger::log_debug(msg, ..., namespace = .MAZAMA_LOG_NAMESPACE)
}

#' @name logger.info
#' @export
#' @importFrom logger log_info
#' @title Python-style logging statements
#' @param msg Message with format strings applied to additional arguments.
#' @param \dots Additional arguments to be formatted.
#' @return No return value.
#' @description After initializing the level-specific log files with \code{logger.setup(...)},
#' this function will generate \code{INFO} level log statements.
#' @note All functionality is built on top of the excellent \pkg{logger} package.
#' @seealso \code{\link{logger.setup}}
logger.info <- function(msg, ...) {
  .stopIfNotInitilized()
  logger::log_info(msg, ..., namespace = .MAZAMA_LOG_NAMESPACE)
}

#' @name logger.warn
#' @export
#' @importFrom logger log_warn
#' @title Python-style logging statements
#' @param msg Message with format strings applied to additional arguments.
#' @param \dots Additional arguments to be formatted.
#' @return No return value.
#' @description After initializing the level-specific log files with \code{logger.setup(...)},
#' this function will generate \code{WARN} level log statements.
#' @note All functionality is built on top of the excellent \pkg{logger} package.
#' @seealso \code{\link{logger.setup}}
logger.warn <- function(msg, ...) {
  .stopIfNotInitilized()
  logger::log_warn(msg, ..., namespace = .MAZAMA_LOG_NAMESPACE)
}

#' @name logger.error
#' @export
#' @importFrom logger log_error
#' @title Python-style logging statements
#' @param msg Message with format strings applied to additional arguments.
#' @param \dots Additional arguments to be formatted.
#' @return No return value.
#' @description After initializing the level-specific log files with \code{logger.setup(...)},
#' this function will generate \code{ERROR} level log statements.
#' @note All functionality is built on top of the excellent \pkg{logger} package.
#' @seealso \code{\link{logger.setup}}
logger.error <- function(msg, ...) {
  .stopIfNotInitilized()
  logger::log_error(msg, ..., namespace = .MAZAMA_LOG_NAMESPACE)
}

#' @name logger.fatal
#' @export
#' @importFrom logger log_fatal
#' @title Python-style logging statements
#' @param msg Message with format strings applied to additional arguments.
#' @param \dots Additional arguments to be formatted.
#' @return No return value.
#' @description After initializing the level-specific log files with \code{logger.setup(...)},
#' this function will generate \code{FATAL} level log statements.
#' @note All functionality is built on top of the excellent \pkg{logger} package.
#' @seealso \code{\link{logger.setup}}
logger.fatal <- function(msg, ...) {
  .stopIfNotInitilized()
  logger::log_fatal(msg, ..., namespace = .MAZAMA_LOG_NAMESPACE)
}

# ------------------------------------------------------------------------------
# Constants (retain existing exported API)
# ------------------------------------------------------------------------------

# Verbatim values from constants (legacy API). These are *not* required to match
# logger's internal numeric values; we map by name (see .logger_map_level()).

#' @docType data
#' @name logLevels
#' @aliases FATAL ERROR WARN INFO DEBUG TRACE
#' @title Log levels
#' @description Log levels matching those historically found in \pkg{futile.logger}.
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
