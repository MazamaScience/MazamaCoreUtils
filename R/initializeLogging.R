#' @title Initialize standard log files
#'
#' @description
#' Convenience function that wraps logging initialization steps common to Mazama
#' Science web services.
#'
#' @param logDir Directory in which to write log files.
#' @return NULL
#'
#' @name initializeLogging
#' @export
#'
initializeLogging <- function(
  logDir = NULL
) {

  # Validate parameters
  stopIfNull(logDir)

  # Copy and old log files
  result <- try({
    # NOTE:  Intentionally create timestamp in host system timezone
    timestamp <- strftime(lubridate::now(tzone = "UTC"), "%Y-%m-%dT%H:%M:%S")
    for (logLevel in c("TRACE", "DEBUG", "INFO", "ERROR")) {
      oldFile <- file.path(logDir, paste0(logLevel, ".log"))
      newFile <- file.path(logDir, paste0(logLevel, ".log.", timestamp))
      if (file.exists(oldFile)) {
        file.rename(oldFile, newFile)
      }
    }
  }, silent = TRUE)
  stopOnError(result, "Could not rename old log files.")

  # Set up logging
  result <- try({
    logger.setup(traceLog = file.path(logDir, "TRACE.log"),
                 debugLog = file.path(logDir, "DEBUG.log"),
                 infoLog = file.path(logDir, "INFO.log"),
                 errorLog = file.path(logDir, "ERROR.log"))
  }, silent = TRUE)
  stopOnError(result, "Could not create log files.")

}


