# Internal file helpers

# Remove files and return the paths that were actually removed. A single warning
# reports any files that could not be removed (e.g. locked or read-only).
.removeFiles <- function(files) {

  removed <- suppressWarnings(file.remove(files))

  if ( !all(removed) ) {
    warning(
      sprintf(
        "%d of %d files could not be removed: %s",
        sum(!removed), length(files), toString(files[!removed])
      ),
      call. = FALSE
    )
  }

  return(files[removed])

}
