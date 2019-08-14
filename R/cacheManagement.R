#' @name manageCache
#'
#' @title Manage the size of a cache
#'
#' @param cacheDir Location of cache directory.
#' @param extensions Vector of file extensions eligible for removal.
#' @param maxCacheSize Maximum cache size in megabytes.
#' @param sortBy Timestamp to sort by when sorting files eligible for removal.
#'   One of \code{atime|ctime|mtime}.
#' @param maxFileAge Maximum age in days of files allowed in the cache.
#'
#' @return Invisibly returns the number of files removed.
#'
#' @description
#' If \code{cacheDir} takes up more than \code{maxCacheSize}
#' megabytes on disk, files will be removed in order of access time by
#' default. Only files matching \code{extensions} are eligible for removal.
#' Files can also be removed in order of change time with \code{sortBy='ctime'}
#' or modification time with \code{sortBy='mtime'}.
#'
#' The \code{maxFileAge} parameter can also be used to remove files that haven't
#' been modified in a certain number of days. Fractional days are allowed. This
#' removal happens without regard to the size of the cache and is useful for
#' removing out-of-date data.
#'
#' It is important to understand precisely what these timestamps
#' represent:
#' \itemize{
#' \item{\code{atime} -- File access time: updated whenever a file is opened.}
#' \item{\code{ctime} -- File change time: updated whenever a file's metadata
#' changes e.g. name, permission, ownership.}
#' \item{\code{mtime} -- file modification time: updated whenever a file's
#' contents change.}
#' }
#'
#' @export
#'
#' @examples
#' # Create a cache directory and fill it with 1.6 MB of data
#' CACHE_DIR <- tempdir()
#' write.csv(matrix(1,400,500), file=file.path(CACHE_DIR,'m1.csv'))
#' write.csv(matrix(2,400,500), file=file.path(CACHE_DIR,'m2.csv'))
#' write.csv(matrix(3,400,500), file=file.path(CACHE_DIR,'m3.csv'))
#' write.csv(matrix(4,400,500), file=file.path(CACHE_DIR,'m4.csv'))
#' for (file in list.files(CACHE_DIR, full.names=TRUE)) {
#'   print(file.info(file)[,c(1,6)])
#' }
#'
#' # Remove files based on access time until we get under 1 MB
#' manageCache(CACHE_DIR, extensions='csv', maxCacheSize=1, sortBy='atime')
#' for (file in list.files(CACHE_DIR, full.names=TRUE)) {
#'   print(file.info(file)[,c(1,6)])
#' }
#'
#' # Or remove files based on modification time
#' manageCache(CACHE_DIR, extensions='csv', maxCacheSize=1, sortBy='mtime')
#' for (file in list.files(CACHE_DIR, full.names=TRUE)) {
#'   print(file.info(file)[,c(1,6)])
#' }

manageCache <- function(cacheDir,
                        extensions = c('html','json','pdf','png'),
                        maxCacheSize = 100,
                        sortBy = 'atime',
                        maxFileAge = NULL) {

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
    }
  }

  # Remove excess files --------------------------------------------------------

  # Use dplyr to order by value specified by sortBy
  if ( !sortBy %in% c("atime", "ctime", "mtime") ) {
    stop("Invalid value for parameter 'sortBy'")
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
