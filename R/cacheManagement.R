#' Manage cache size
#'
#' Remove old or excess files from a cache directory.
#'
#' Files are eligible for removal when their extension matches `extensions`.
#' Matching is case-sensitive and extensions may be supplied with or without a
#' leading dot.
#'
#' Files can be removed for two reasons:
#'
#' \itemize{
#'   \item files older than `maxFileAge` days are removed first
#'   \item if the remaining cache exceeds `maxCacheSize`, additional files are
#'   removed until the cache is under the requested size
#' }
#'
#' When removing files to satisfy `maxCacheSize`, files are ordered by the
#' timestamp specified by `sortBy`.
#'
#' @param cacheDir Location of cache directory.
#' @param extensions Vector of file extensions eligible for removal.
#' @param maxCacheSize Maximum cache size in megabytes.
#' @param sortBy Timestamp used to order files for size-based removal. One of
#'   `"atime"`, `"ctime"`, or `"mtime"`.
#' @param maxFileAge Maximum file age in days. Files with modification times
#'   older than this value are removed regardless of cache size. Fractional days
#'   are allowed.
#'
#' @return
#' Invisibly returns the number of files removed.
#'
#' @details
#' Timestamp meanings are:
#'
#' \describe{
#'   \item{`atime`}{File access time, updated when a file is opened.}
#'   \item{`ctime`}{File change time, updated when file metadata changes.}
#'   \item{`mtime`}{File modification time, updated when file contents change.}
#' }
#'
#' @examples
#' CACHE_DIR <- tempdir()
#'
#' write.csv(matrix(1, 400, 500), file = file.path(CACHE_DIR, "m1.csv"))
#' write.csv(matrix(2, 400, 500), file = file.path(CACHE_DIR, "m2.csv"))
#' write.csv(matrix(3, 400, 500), file = file.path(CACHE_DIR, "m3.csv"))
#' write.csv(matrix(4, 400, 500), file = file.path(CACHE_DIR, "m4.csv"))
#'
#' for (file in list.files(CACHE_DIR, pattern = "\\.csv$", full.names = TRUE)) {
#'   print(file.info(file)[, c("size", "mtime")])
#' }
#'
#' # Remove files based on access time until the cache is under 1 MB
#' manageCache(
#'   CACHE_DIR,
#'   extensions = "csv",
#'   maxCacheSize = 1,
#'   sortBy = "atime"
#' )
#'
#' for (file in list.files(CACHE_DIR, pattern = "\\.csv$", full.names = TRUE)) {
#'   print(file.info(file)[, c("size", "mtime")])
#' }
#'
#' @name manageCache
#' @importFrom rlang .data
#' @export
#'

manageCache <- function(
  cacheDir = NULL,
  extensions = c('html','json','pdf','png'),
  maxCacheSize = 100,
  sortBy = 'atime',
  maxFileAge = NULL
) {

  # Validate parameters --------------------------------------------------------

  stopIfNull(cacheDir)
  stopIfNull(extensions)
  stopIfNull(maxCacheSize)
  stopIfNull(sortBy)

  # Get file info --------------------------------------------------------------

  # Convert incoming size from megabytes to bytes
  maxCacheSize <- as.numeric(maxCacheSize) * 1e6

  # Get all files appropriate for deletion
  filesList <- list()
  for ( extension in extensions ) {
    extension <- stringr::str_replace(extension,'^\\.','') # replace initial '.'
    pattern <- paste0('\\.',extension,'$')
    filesList[[extension]] <- list.files(cacheDir,
                                         pattern=pattern,
                                         full.names=TRUE)
  }
  cacheFiles <- unlist(filesList, use.names=FALSE)

  # Create a dataframe with access times and file sizes
  cacheDF <- file.info(cacheFiles)
  cacheDF$file <- rownames(cacheDF)

  # Remove old files -----------------------------------------------------------

  if ( is.null(maxFileAge) ) {
    ageRemovalCount <- 0
  } else {
    expiration <- lubridate::now(tzone = "UTC") - lubridate::ddays(maxFileAge)
    removalDF <- dplyr::filter(cacheDF, cacheDF$mtime < expiration)
    ageRemovalCount <- nrow(removalDF)
    if ( ageRemovalCount > 0 ) {
      file.remove(removalDF$file)
      # Remove deleted files before size-based cleanup.
      cacheDF <-
        cacheDF %>%
        dplyr::filter(!.data$file %in% removalDF$file)
    }
  }

  # Remove excess files --------------------------------------------------------

  # Use dplyr to order by value specified by sortBy
  if ( !sortBy %in% c("atime", "ctime", "mtime") ) {
    stop("invalid value for parameter 'sortBy'")
  } else {
    sizeByDF <- dplyr::arrange(cacheDF, dplyr::desc(.data[[sortBy]]))
  }

  # Compute a running total
  sizeByDF$cumulativeSize <- cumsum(sizeByDF$size)

  # Remove all files associated with cumulativeSize > maxCacheSize
  removalMask <- sizeByDF$cumulativeSize > maxCacheSize
  removalFiles <- sizeByDF$file[removalMask]
  sizeRemovalCount <- length(removalFiles)
  if ( sizeRemovalCount > 0 ) {
    file.remove(removalFiles)
  }

  # Return ---------------------------------------------------------------------

  removalCount <- ageRemovalCount + sizeRemovalCount

  return(invisible(removalCount))

}
