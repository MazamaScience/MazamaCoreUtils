#' Create a POSIXct time range
#'
#' Create an ordered two-element `POSIXct` time range from start and end
#' datetime values.
#'
#' Input values are converted with [parseDatetime()] using the required
#' `timezone` argument. The resulting start and end times are sorted so the
#' earlier time is always returned first.
#'
#' By default, both times are rounded down with [lubridate::floor_date()] using
#' the requested `unit`. Set `ceilingStart = TRUE` or `ceilingEnd = TRUE` to
#' round either endpoint up with [lubridate::ceiling_date()] instead.
#'
#' @param starttime Desired start datetime.
#' @param endtime Desired end datetime.
#' @param timezone Olson timezone used to interpret incoming datetimes.
#' @param unit Unit used for rounding. Passed to [lubridate::floor_date()] or
#'   [lubridate::ceiling_date()].
#' @param ceilingStart Logical specifying whether to round the start time up
#'   instead of down.
#' @param ceilingEnd Logical specifying whether to round the end time up instead
#'   of down.
#'
#' @return
#' Two-element `POSIXct` vector ordered from earliest to latest.
#'
#' @inheritSection dateRange POSIXct inputs
#'
#' @examples
#' timeRange(
#'   starttime = "2019-01-08 10:12:15",
#'   endtime = 20190109102030,
#'   timezone = "UTC"
#' )
#'
#' timeRange(
#'   starttime = "2019-01-08 10:12:15",
#'   endtime = "2019-01-09 10:20:30",
#'   timezone = "UTC",
#'   unit = "hour"
#' )
#'
#' @export
#'
timeRange <- function(
  starttime = NULL,
  endtime = NULL,
  timezone = NULL,
  unit = "sec",
  ceilingStart = FALSE,
  ceilingEnd = FALSE
) {

  # ----- Validate parameters --------------------------------------------------

  stopIfNull(starttime)
  stopIfNull(endtime)
  stopIfNull(timezone)
  setIfNull(unit, "sec")
  setIfNull(ceilingStart, FALSE)
  setIfNull(ceilingEnd, FALSE)

  if ( !timezone %in% base::OlsonNames() )
    stop(sprintf("'timezone = %s' is not found in OlsonNames()", timezone))

  # ----- Process datetimes ----------------------------------------------------

  # Guarantee conversion to POSIXct
  starttime <- parseDatetime(starttime, timezone = timezone)
  endtime <- parseDatetime(endtime, timezone = timezone)

  # Guarantee proper ordering
  timeRange <- sort(c(starttime, endtime))

  # Floor/Ceiling to nearest unit
  if ( ceilingStart ) {
    timeRange[1] <- lubridate::ceiling_date(timeRange[1], unit = unit)
  } else {
    timeRange[1] <- lubridate::floor_date(timeRange[1], unit = unit)
  }

  if ( ceilingEnd ) {
    timeRange[2] <- lubridate::ceiling_date(timeRange[2], unit = unit)
  } else {
    timeRange[2] <- lubridate::floor_date(timeRange[2], unit = unit)
  }

  # ----- Return ---------------------------------------------------------------

  return(timeRange)

}
