# Internal helper shared by the date-time functions.
#
# Stops with a clear message unless 'timezone' is a single character string
# found in base::OlsonNames(). Returns 'timezone' invisibly so the call can be
# used as a plain validation statement.
.validateTimezone <- function(timezone) {

  if ( !is.character(timezone) || length(timezone) != 1 )
    stop("argument 'timezone' must be a character string of length one")

  if ( !timezone %in% base::OlsonNames() )
    stop(sprintf("'timezone = %s' is not found in OlsonNames()", timezone))

  invisible(timezone)

}
