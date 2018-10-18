#' @name initializeLogging
#' @export
#' @title Initialize standard log files
#' @param logDir Directory in which to write log files.
#' @return No return value.
#' @description Convenience function that wraps logging initialization steps
#' common to Mazama Science web services:
#' 
#' \preformatted{
#' result <- try({
#'   Copy and old log files
#'   timestamp <- strftime(lubridate::now(), "%Y-%m-%dT%H:%M:%S")
#'   for ( logLevel in c("TRACE","DEBUG","INFO","ERROR") ) {
#'     oldFile <- file.path(logDir,paste0(logLevel,".log"))
#'     newFile <- file.path(logDir,paste0(logLevel,".log.",timestamp))
#'     if ( file.exists(oldFile) ) {
#'       file.rename(oldFile, newFile)
#'     }
#'   }
#' }, silent=TRUE)
#' stopOnError(result, "Could not rename old log files.")
#' 
#' result <- try({
#'   # Set up logging
#'   logger.setup(traceLog = file.path(logDir, "TRACE.log"),
#'                debugLog=file.path(logDir, "DEBUG.log"),
#'                infoLog=file.path(logDir, "INFO.log"),
#'                errorLog=file.path(logDir, "ERROR.log"))
#' }, silent=TRUE)
#' stopOnError(result, "Could not create log files.")
#' }

initializeLogging <- function(logDir= NULL) {
  
  if ( is.null(logDir) ) {
    stop("Required parameter 'logDir' is missing.", call.=FALSE)
  }
  
  result <- try({
    # Copy and old log files
    timestamp <- strftime(lubridate::now(), "%Y-%m-%dT%H:%M:%S")
    for ( logLevel in c("TRACE","DEBUG","INFO","ERROR") ) {
      oldFile <- file.path(logDir,paste0(logLevel,".log"))
      newFile <- file.path(logDir,paste0(logLevel,".log.",timestamp))
      if ( file.exists(oldFile) ) {
        file.rename(oldFile, newFile)
      }
    }
  }, silent=TRUE)
  stopOnError(result, "Could not rename old log files.")
  
  result <- try({
    # Set up logging
    logger.setup(traceLog = file.path(logDir, "TRACE.log"),
                 debugLog=file.path(logDir, "DEBUG.log"),
                 infoLog=file.path(logDir, "INFO.log"),
                 errorLog=file.path(logDir, "ERROR.log"))
  }, silent=TRUE)
  stopOnError(result, "Could not create log files.")
  
}
