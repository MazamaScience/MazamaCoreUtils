#' @title Initialize standard log files
#'
#' @description
#' Convenience function that wraps common logging initialization steps.
#'
#' @param logDir Directory in which to write log files.
#' @param filePrefix Character string prepended to log files.
#' @param createDir Logical specifying whether to create a missing \code{logDir}
#' or issue an error message.
#'
#' @return NULL
#'
#' @name initializeLogging
#' @export
#'
initializeLogging <- function(
  logDir = NULL,
  filePrefix = "",
  createDir = TRUE
) {

  # ----- Validate parameters --------------------------------------------------

  stopIfNull(logDir)
  setIfNull(filePrefix, "")
  setIfNull(createDir, TRUE)

  # ----- Create Directory -----------------------------------------------------

  if ( !dir.exists(logDir) ) {
    if ( createDir ) {
      dir.create(logDir, showWarnings = FALSE, recursive = TRUE)
    } else {
      stop(sprintf("'logDir = %s' is not found and 'createDir = FALSE'", logDir ))
    }
  }

  # ----- Copy old log files ---------------------------------------------------

  result <- try({
    # NOTE:  Intentionally create timestamp in host system timezone
    timestamp <- strftime(lubridate::now(tzone = "UTC"), "%Y-%m-%dT%H:%M:%S")
    for (logLevel in c("TRACE", "DEBUG", "INFO", "ERROR")) {
      oldFile <- file.path(logDir, paste0(filePrefix, logLevel, ".log"))
      newFile <- file.path(logDir, paste0(filePrefix, logLevel, ".log.", timestamp))
      if ( file.exists(oldFile) ) {
        file.rename(oldFile, newFile)
      }
    }
  }, silent = TRUE)
  stopOnError(result, "could not rename old log files")

  # ----- Set up logging -------------------------------------------------------

  result <- try({
    logger.setup(
      traceLog = file.path(logDir, "TRACE.log"),
      debugLog = file.path(logDir, "DEBUG.log"),
      infoLog = file.path(logDir, "INFO.log"),
      errorLog = file.path(logDir, "ERROR.log")
    )
  }, silent = TRUE)
  stopOnError(result, "could not create log files")

}


