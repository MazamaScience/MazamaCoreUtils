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
    # NOTE:  Intentionally create timestamp in host system timezone
    timestamp <- strftime(lubridate::now(tzone = "UTC"), "%Y-%m-%dT%H:%M:%S")
    for (logLevel in c("TRACE", "DEBUG", "INFO", "ERROR")) {
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
      errorLog = file.path(logDir, sprintf("%sERROR.log", filePrefix))
    )
  }, silent = TRUE) %>%
  stopOnError("could not create log files")

}


