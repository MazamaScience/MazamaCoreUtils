#' Initialize standard log files
#'
#' Create a standard set of MazamaCoreUtils log files.
#'
#' This convenience function creates or validates a log directory, archives any
#' existing standard log files by appending a UTC timestamp, and then initializes
#' logging with [logger.setup()].
#'
#' Standard log files include:
#'
#' \preformatted{
#' TRACE.log
#' DEBUG.log
#' INFO.log
#' WARN.log
#' ERROR.log
#' }
#'
#' When `filePrefix` is supplied, it is prepended to each log file name.
#'
#' @param logDir Directory in which to write log files.
#' @param filePrefix Character string prepended to log file names.
#' @param createDir Logical specifying whether to create `logDir` if it does
#'   not already exist.
#'
#' @return
#' No return value. Called for side effects.
#'
#' @name initializeLogging
#' @export
#'
#' @seealso
#' [logger.setup()]
#'
initializeLogging <- function(
  logDir = NULL,
  filePrefix = "",
  createDir = TRUE
) {

  # ----- Validate parameters --------------------------------------------------

  stopIfNull(logDir)
  filePrefix <- setIfNull(filePrefix, "")
  createDir <- setIfNull(createDir, TRUE)

  # ----- Create Directory -----------------------------------------------------

  if ( !dir.exists(logDir) ) {
    if ( createDir ) {
      dir.create(logDir, showWarnings = FALSE, recursive = TRUE)
    } else {
      stop(sprintf("'logDir = %s' is not found and 'createDir = FALSE'", logDir ))
    }
  }

  # ----- Copy old log files ---------------------------------------------------

  try({
    # NOTE:  Intentionally create timestamp in UTC
    timestamp <- strftime(lubridate::now(tzone = "UTC"), "%Y-%m-%dT%H:%M:%SZ")
    for (logLevel in c("TRACE", "DEBUG", "INFO", "WARN", "ERROR")) {
      oldFile <- file.path(logDir, sprintf("%s%s.log", filePrefix, logLevel))
      newFile <- file.path(logDir, sprintf("%s%s.log.%s", filePrefix, logLevel, timestamp))
      if ( file.exists(oldFile) ) {
        file.rename(oldFile, newFile)
      }
    }
  }, silent = TRUE) %>%
  stopOnError("could not rename old log files")

  # ----- Set up logging -------------------------------------------------------

  try({
    logger.setup(
      traceLog = file.path(logDir, sprintf("%sTRACE.log", filePrefix)),
      debugLog = file.path(logDir, sprintf("%sDEBUG.log", filePrefix)),
      infoLog = file.path(logDir, sprintf("%sINFO.log", filePrefix)),
      warnLog = file.path(logDir, sprintf("%sWARN.log", filePrefix)),
      errorLog = file.path(logDir, sprintf("%sERROR.log", filePrefix))
    )
  }, silent = TRUE) %>%
  stopOnError("could not create log files")

  invisible(NULL)
}


