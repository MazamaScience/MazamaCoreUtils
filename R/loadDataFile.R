#' @export
#'
#' @title Load R data from URL or local file
#'
#' @param filename Name of the R data file to be loaded.
#' @param dataUrl Remote URL directory for data files.
#' @param dataDir Local disk directory containing data files.
#' @param priority First data source to attempt if both are supplied.
#' @return A data object.
#'
#' @description Loads pre-generated R binary (".rda") files from a URL or a local
#' directory. This function is intended to be called by other \code{~_load()}
#' functions and can remove internet latencies when local versions of data are
#' available.
#'
#' If both \code{dataUrl} and \code{dataDir} are provided, an attempt will be
#' made to load data from the source specified by \code{priority} with the
#' other source used as a backup.
#'
#' @examples
#' \dontrun{
#' library(MazamaCoreUtils)
#'
#' filename = "USCensusStates_02.rda"
#' dir = "~/Data/Spatial"
#' url = "http://data.mazamascience.com/MazamaSpatialUtils/Spatial_0.8"
#'
#' # Load local file
#' USCensusStates = loadDataFile(filename, dataDir = dir)
#'
#' # Load remote file
#' USCensusStates = loadDataFile(filename, dataUrl = url)
#'
#' # Load local file with remote file as backup
#' USCensusStates =
#'   loadDataFile(filename, dataDir = dir, dataUrl = url, priority = "dataDir")
#'
#' # Load remote file with local file as backup
#' USCensusStates =
#'   loadDataFile(filename, dataDir = dir, dataUrl = url, priority = "dataUrl")
#'
#' }


loadDataFile <- function(
    filename = NULL,
    dataUrl = NULL,
    dataDir = NULL,
    priority = c("dataDir", "dataUrl")
) {

  # ----- Validate parameters --------------------------------------------------

  MazamaCoreUtils::stopIfNull(filename)

  if ( is.null(dataUrl) && is.null(dataDir) ) {
    stop("either 'dataUrl' or 'dataDir' must be specified")
  }

  priority <- match.arg(priority)

  # ----- Load the data --------------------------------------------------------

  # * dataDir priority -----

  if ( !is.null(dataDir) && (is.null(dataUrl) || priority == "dataDir") ) {

    # Don't stop yet
    result <- try({
      loadedData <- .loadFromDir(filename, dataDir)
    }, silent = TRUE)

    if ( "try-error" %in% class(result) ) {
      if ( is.null(dataUrl) ) {
        stop(toString(result), call. = FALSE)
      } else {
        # Load form URL or stop
        try({
          loadedData <- .loadFromUrl(filename, dataUrl)
        }, silent = TRUE) %>%
          stopOnError("data file could not be loaded from dataDir or dataUrl")
      }
    }

  }

  # * dataUrl priority -----

  if ( !is.null(dataUrl) && (is.null(dataDir) || priority == "dataUrl") ) {

    # Don't stop yet
    result <- try({
      loadedData <- .loadFromUrl(filename, dataUrl)
    }, silent = TRUE)

    if ( "try-error" %in% class(result) ) {
      if ( is.null(dataDir) ) {
        stop(toString(result), call. = FALSE)
      } else {
        # Load from directory or stop
        try({
          loadedData <- .loadFromDir(filename, dataDir)
        }, silent = TRUE) %>%
          stopOnError("data file could not be loaded from dataDir or dataUrl")
      }
    }

  }

  # ----- Return ---------------------------------------------------------------

  return(loadedData)

}

# ===== INTERNAL FUNCTIONS =====================================================

.loadFromDir <- function(filename, dataDir) {

  dataDir <- path.expand(dataDir)

  if ( !dir.exists(dataDir) )
    stop(sprintf("dataDir '%s' does not exist.", dataDir), call. = FALSE)

  filepath <- file.path(dataDir, filename)

  result <- try({
    suppressWarnings({
      objectName <- load(filepath)
    })
  }, silent = TRUE)

  if ( "try-error" %in% class(result) ) {
    stop(sprintf("data file could not be loaded from: %s", filepath), call. = FALSE)
  }

  # No error
  loadedData <- get(objectName)
  return(loadedData)

}


.loadFromUrl <- function(filename, dataUrl) {

  filepath <- file.path(dataUrl, filename)

  # Define a 'connection' object so we can close it no matter what happens
  conn <- url(filepath)
  result <- try({
    suppressWarnings({
      objectName <- load(conn)
    })
  }, silent = TRUE)
  close(conn)

  if ( "try-error" %in% class(result) ) {
    stop(sprintf("data file could not be loaded from: %s", filepath), call. = FALSE)
  }

  # No error
  loadedData <- get(objectName)
  return(loadedData)

}
