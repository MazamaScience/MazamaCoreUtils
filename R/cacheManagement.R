#' @name manageCache
#' @export
#' @title Manage the Size of a Cache
#' @param cacheDir location of cache directory
#' @param extensions vector of file extensions eligible for removal
#' @param maxCacheSize maximum cache size in megabytes
#' @param sortBy value to sort by when sorting eligible files for removal
#' @return Invisibly returns the number of files removed.
#' @description If \code{cacheDir} takes up more than \code{maxCacheSize}
#' megabytes on disk, files wll be removed in order of oldest access time 
#' (\code{sortBy='a'}), modification time (\code{sortBy='m'}), or creation 
#' time, (\code{sortBy='c'}). Only files matching \code{extensions} 
#' are eligible for removal.
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
#' # Remove files based on last access time until we get under 1 MB
#' manageCache(CACHE_DIR, extensions='csv', maxCacheSize=1, sortBy='a')
#' for (file in list.files(CACHE_DIR, full.names=TRUE)) {
#'   print(file.info(file)[,c(1,6)])
#' }

if (FALSE) {
  
  cacheDir <- "."
  extensions <- "testing"
  maxCacheSize <- 0.001
  sortBy <- 'c'
  write.csv(matrix(1,400,500), file=file.path(cacheDir,'m1.testing'))
  write.csv(matrix(2,400,500), file=file.path(cacheDir,'m2.testing'))
  write.csv(matrix(3,400,500), file=file.path(cacheDir,'m3.testing'))
  write.csv(matrix(4,400,500), file=file.path(cacheDir,'m4.testing'))
  
}

manageCache <- function(cacheDir,
                        extensions=c('html','json','pdf','png'),
                        maxCacheSize=100,
                        sortBy='a') {
  
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
  
  # Use dplyr to order by value specified by sortBy
  if ( sortBy == 'a' ) {
    sizeByDF <- dplyr::arrange_(cacheDF, "desc(atime)")
  } else if ( sortBy == 'c') {
    sizeByDF <- dplyr::arrange_(cacheDF, "desc(ctime)")
  } else if ( sortBy == 'm') {
    sizeByDF <- dplyr::arrange_(cacheDF, "desc(mtime)")
  } else {
    stop("Invalid value for parameter 'sortBy'")
  }
  
  # Compute a running total
  sizeByDF$cumulativeSize <- cumsum(sizeByDF$size)
  
  # Remove all files associated with cumulativeSize > maxCacheSize
  removalMask <- sizeByDF$cumulativeSize > maxCacheSize
  removalFiles <- sizeByDF$file[removalMask]
  if ( length(removalFiles) > 0 ) {
    file.remove(removalFiles)
  }
  
  return(invisible(length(removalFiles)))
  
}
