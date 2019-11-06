#' @export
#'
#' @title Create a POSIXct time range
#'
#' @param starttime Desired start datetime (ISO 8601).
#' @param endtime Desired end datetime (ISO 8601).
#' @param timezone Olson timezone used to interpret dates (required).
#' @param unit Units used to determine time at end-of-day.
#' @param ceilingEnd Logical instruction to apply
#'   \code{\link[lubridate]{ceiling_date}} to the \code{enddate} rather than
#'   \code{\link[lubridate]{floor_date}}
#'
#' @description
#' Uses incoming parameters to return a pair of \code{POSIXct} times in the
#' proper order. Both start and end times will have \code{lubridate::floor_date()}
#' applied to get the nearest \code{unit} unless \code{ceilingEnd = TRUE} in
#' which case the end time will will have \code{lubridate::ceiling_date()}
#' applied.
#'
#' The required \code{timezone} parameter must be one of those found in
#' \code{\link[base]{OlsonNames}}.
#'
#' Dates can be anything that is understood by
#' \code{lubrdiate::parse_date_time()} including either of the following
#' recommended formats:
#'
#' \itemize{
#'   \item{\code{"YYYYmmddHH[MMSS]"}}
#'   \item{\code{"YYYY-mm-dd HH:MM:SS"}}
#' }
#'
#' @inheritSection dateRange POSIXct inputs
#'
#' @return A vector of two \code{POSIXct}s.
#'
#' @examples
#' timeRange("2019-01-08 10:12:15", 20190109102030, timezone = "UTC")
timeRange <- function(
  starttime = NULL,
  endtime = NULL,
  timezone = NULL,
  unit = "sec",
  ceilingEnd = FALSE
) {

  # ----- Validate parameters --------------------------------------------------

  stopIfNull(starttime)
  stopIfNull(endtime)
  stopIfNull(timezone)
  stopIfNull(unit)
  stopIfNull(ceilingEnd)

  if ( !timezone %in% base::OlsonNames() )
    stop(paste0("Timezone '", timezone, "' is not recognized."))

  # ----- Process datetimes ----------------------------------------------------

  # Guarantee conversion to POSIXct
  starttime <- parseDatetime(starttime, timezone = timezone)
  endtime <- parseDatetime(endtime, timezone = timezone)

  # Guarantee proper ordering
  timeRange <- sort(c(starttime, endtime))

  # Floor/Ceiling to nearest unit
  timeRange[1] <- lubridate::floor_date(timeRange[1], unit = unit)
  if ( ceilingEnd ) {
    timeRange[2] <- lubridate::ceiling_date(timeRange[2], unit = unit)
  } else {
    timeRange[2] <- lubridate::floor_date(timeRange[2], unit = unit)
  }

  # ----- Return ---------------------------------------------------------------

  return(timeRange)

}
