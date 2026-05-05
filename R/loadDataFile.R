#' Load R data from a URL or local file
#'
#' Load a pre-generated R binary data file from either a local directory or a
#' remote URL.
#'
#' This function is intended for use by package-level `*_load()` helper
#' functions. It allows locally cached data files to be used when available,
#' avoiding unnecessary internet access.
#'
#' If both `dataDir` and `dataUrl` are provided, `priority` determines which
#' source is tried first. If loading from the first source fails, the other
#' source is used as a fallback.
#'
#' @param filename Name of the `.rda` file to load.
#' @param dataUrl Remote URL directory containing data files.
#' @param dataDir Local directory containing data files.
#' @param priority First data source to try when both `dataDir` and `dataUrl`
#'   are supplied.
#'
#' @return
#' Object loaded from the `.rda` file.
#'
#' @examples
#' \dontrun{
#' filename <- "USCensusStates_02.rda"
#' dataDir <- "~/Data/Spatial"
#' dataUrl <- "http://data.mazamascience.com/MazamaSpatialUtils/Spatial_0.8"
#'
#' # Load local file
#' USCensusStates <- loadDataFile(filename, dataDir = dataDir)
#'
#' # Load remote file
#' USCensusStates <- loadDataFile(filename, dataUrl = dataUrl)
#'
#' # Load local file with remote file as backup
#' USCensusStates <- loadDataFile(
#'   filename,
#'   dataDir = dataDir,
#'   dataUrl = dataUrl,
#'   priority = "dataDir"
#' )
#'
#' # Load remote file with local file as backup
#' USCensusStates <- loadDataFile(
#'   filename,
#'   dataDir = dataDir,
#'   dataUrl = dataUrl,
#'   priority = "dataUrl"
#' )
#' }
#'
#' @export
#'
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
  on.exit(close(conn), add = TRUE)
  result <- try({
    suppressWarnings({
      objectName <- load(conn)
    })
  }, silent = TRUE)

  if ( "try-error" %in% class(result) ) {
    stop(sprintf("data file could not be loaded from: %s", filepath), call. = FALSE)
  }

  # No error
  loadedData <- get(objectName)
  return(loadedData)

}
