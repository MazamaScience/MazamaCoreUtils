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

#' Set up Python-style logging
#'
#' Configure level-specific log files using the package logging API.
#'
#' Logging is built on top of the [logger](https://daroczig.github.io/logger/)
#' package while retaining the historical MazamaCoreUtils logging interface.
#'
#' Separate log files can be created for different log levels so that, for
#' example, an `errorLog` contains only `ERROR` and `FATAL` messages while a
#' `debugLog` contains `DEBUG` messages as well as all higher-severity messages.
#'
#' Any log file argument left as `NULL` is disabled and no file will be created
#' for that level.
#'
#' After initialization, logging statements can be generated with:
#' `logger.trace()`, `logger.debug()`, `logger.info()`,
#' `logger.warn()`, `logger.error()`, and `logger.fatal()`.
#'
#' @param traceLog File path receiving `TRACE` messages.
#' @param debugLog File path receiving `DEBUG` messages.
#' @param infoLog File path receiving `INFO` messages.
#' @param warnLog File path receiving `WARN` messages.
#' @param errorLog File path receiving `ERROR` messages.
#' @param fatalLog File path receiving `FATAL` messages.
#'
#' @return
#' No return value. Called for side effects.
#'
#' @details
#' Log messages are formatted with:
#'
#' \preformatted{
#' LEVEL [YYYY-MM-DD HH:MM:SS UTC] message
#' }
#'
#' Console logging is enabled by default only for `FATAL` messages. Use
#' [logger.setLevel()] to display additional log messages in the console.
#'
#' @note
#' All functionality is implemented with the excellent \pkg{logger} package.
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
#' # Create three log files
#' logger.setup(
#'   debugLog = "debug.log",
#'   infoLog = "info.log",
#'   errorLog = "error.log"
#' )
#'
#' # Generate log messages
#' logger.trace("trace statement #%d", 1)
#' logger.debug("debug statement")
#' logger.info("info statement %s %s", "with", "arguments")
#' logger.warn("warn statement: %s", "about to try something risky")
#'
#' result <- try(1 / "a", silent = TRUE)
#' logger.error("error message: %s", geterrmessage())
#' logger.fatal("fatal statement: %s", "THE END")
#'
#' cat(readLines("debug.log"), sep = "\n")
#' cat(readLines("info.log"), sep = "\n")
#' cat(readLines("error.log"), sep = "\n")
#' }
#'
#' @seealso
#' [logger.trace()], [logger.debug()], [logger.info()],
#' [logger.warn()], [logger.error()], [logger.fatal()]
#'
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

#' Check whether logging has been initialized
#'
#' Determine whether [logger.setup()] has already been called.
#'
#' This function is useful in package code that conditionally emits log
#' statements only when logging has been configured.
#'
#' @return
#' Logical scalar indicating whether logging has been initialized.
#'
#' @name logger.isInitialized
#' @export
#'
#' @examples
#' \dontrun{
#' logger.isInitialized()
#'
#' logger.setup()
#'
#' logger.isInitialized()
#' }
#'
#' @seealso
#' [logger.setup()]
#'
logger.isInitialized <- function() {
  isTRUE(getOption("MazamaCoreUtils.logger.initialized", FALSE))
}

#' Set console log level
#'
#' Set the minimum log level displayed in the console.
#'
#' By default, only `FATAL` messages are displayed in the console. This
#' function allows users to display additional log messages interactively.
#'
#' Available log levels are:
#'
#' \preformatted{
#' TRACE
#' DEBUG
#' INFO
#' WARN
#' ERROR
#' FATAL
#' }
#'
#' @param level Logging threshold level.
#'
#' @return
#' No return value. Called for side effects.
#'
#' @note
#' All functionality is implemented with the excellent \pkg{logger} package.
#'
#' @name logger.setLevel
#' @importFrom logger log_threshold
#' @export
#'
#' @examples
#' \dontrun{
#' # Enable console logging
#' logger.setup()
#'
#' # Show DEBUG and higher messages in the console
#' logger.setLevel(DEBUG)
#' }
#'
#' @seealso
#' [logger.setup()]
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

#' Python-style logging statements
#'
#' Emit a `TRACE` level log message.
#'
#' Logging must first be initialized with [logger.setup()].
#'
#' @name logger.trace
#' @param msg Message with optional format strings.
#' @param ... Additional arguments passed to `sprintf()` formatting.
#'
#' @return
#' No return value. Called for side effects.
#'
#' @seealso
#' [logger.setup()]
#'
#' @export
#' @importFrom logger log_trace
logger.trace <- function(msg, ...) {
  .stopIfNotInitilized()
  logger::log_trace(msg, ..., namespace = .MAZAMA_LOG_NAMESPACE)
}

#' Python-style logging statements
#'
#' Emit a `DEBUG` level log message.
#'
#' Logging must first be initialized with [logger.setup()].
#'
#' @name logger.debug
#' @param msg Message with optional format strings.
#' @param ... Additional arguments passed to `sprintf()` formatting.
#'
#' @return
#' No return value. Called for side effects.
#'
#' @seealso
#' [logger.setup()]
#'
#' @export
#' @importFrom logger log_debug
#'
logger.debug <- function(msg, ...) {
  .stopIfNotInitilized()
  logger::log_debug(msg, ..., namespace = .MAZAMA_LOG_NAMESPACE)
}

#' Python-style logging statements
#'
#' Emit an `INFO` level log message.
#'
#' Logging must first be initialized with [logger.setup()].
#'
#' @name logger.info
#' @param msg Message with optional format strings.
#' @param ... Additional arguments passed to `sprintf()` formatting.
#'
#' @return
#' No return value. Called for side effects.
#'
#' @seealso
#' [logger.setup()]
#'
#' @export
#' @importFrom logger log_info
#'
logger.info <- function(msg, ...) {
  .stopIfNotInitilized()
  logger::log_info(msg, ..., namespace = .MAZAMA_LOG_NAMESPACE)
}

#' Python-style logging statements
#'
#' Emit a `WARN` level log message.
#'
#' Logging must first be initialized with [logger.setup()].
#'
#' @name logger.warn
#' @param msg Message with optional format strings.
#' @param ... Additional arguments passed to `sprintf()` formatting.
#'
#' @return
#' No return value. Called for side effects.
#'
#' @seealso
#' [logger.setup()]
#'
#' @export
#' @importFrom logger log_warn
#'
logger.warn <- function(msg, ...) {
  .stopIfNotInitilized()
  logger::log_warn(msg, ..., namespace = .MAZAMA_LOG_NAMESPACE)
}

#' Python-style logging statements
#'
#' Emit an `ERROR` level log message.
#'
#' Logging must first be initialized with [logger.setup()].
#'
#' @name logger.error
#' @param msg Message with optional format strings.
#' @param ... Additional arguments passed to `sprintf()` formatting.
#'
#' @return
#' No return value. Called for side effects.
#'
#' @seealso
#' [logger.setup()]
#'
#' @export
#' @importFrom logger log_error
#'
logger.error <- function(msg, ...) {
  .stopIfNotInitilized()
  logger::log_error(msg, ..., namespace = .MAZAMA_LOG_NAMESPACE)
}

#' Python-style logging statements
#'
#' Emit a `FATAL` level log message.
#'
#' Logging must first be initialized with [logger.setup()].
#'
#' @name logger.fatal
#' @param msg Message with optional format strings.
#' @param ... Additional arguments passed to `sprintf()` formatting.
#'
#' @return
#' No return value. Called for side effects.
#'
#' @seealso
#' [logger.setup()]
#'
#' @export
#' @importFrom logger log_fatal
#'
logger.fatal <- function(msg, ...) {
  .stopIfNotInitilized()
  logger::log_fatal(msg, ..., namespace = .MAZAMA_LOG_NAMESPACE)
}

# ------------------------------------------------------------------------------
# Constants (retain existing exported API)
# ------------------------------------------------------------------------------

# Verbatim values from constants (legacy API). These are *not* required to match
# logger's internal numeric values; we map by name (see .logger_map_level()).

#' Log levels
#'
#' Logging level constants used by the MazamaCoreUtils logging API.
#'
#' Available log levels include:
#'
#' \preformatted{
#' FATAL
#' ERROR
#' WARN
#' INFO
#' DEBUG
#' TRACE
#' }
#'
#' These constants are retained for backwards compatibility with the original
#' MazamaCoreUtils logging system.
#'
#' @docType data
#' @name logLevels
#' @aliases FATAL ERROR WARN INFO DEBUG TRACE
#' @export
#'
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
