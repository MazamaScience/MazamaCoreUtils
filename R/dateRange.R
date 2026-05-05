#' Create a POSIXct date range
#'
#' Create a two-element `POSIXct` vector representing a date/time range in a
#' specified timezone.
#'
#' The returned range is ordered from earliest to latest. The first element
#' represents the beginning of the requested date range and the second element
#' represents the end of the requested date range at the requested temporal
#' precision.
#'
#' By default, the returned end time is one `unit` *before* the beginning of
#' `enddate`. For example:
#'
#' \preformatted{
#' dateRange(20190101, 20190102, timezone = "UTC")
#' [1] "2019-01-01 00:00:00 UTC"
#' [2] "2019-01-01 23:59:59 UTC"
#' }
#'
#' Setting `ceilingEnd = TRUE` includes the entirety of `enddate`:
#'
#' \preformatted{
#' dateRange(
#'   20190101,
#'   20190101,
#'   timezone = "UTC",
#'   ceilingEnd = TRUE
#' )
#' [1] "2019-01-01 00:00:00 UTC"
#' [2] "2019-01-01 23:59:59 UTC"
#' }
#'
#' The `ceilingEnd` argument addresses ambiguity in phrases such as
#' `"August 1-8"`. With `ceilingEnd = FALSE` (default), the range extends
#' through the end of August 7, stopping at the midnight boundary where August 8
#' begins. With `ceilingEnd = TRUE`, the range
#' extends through the end of August 8.
#'
#' Input dates are parsed with [parseDatetime()] using the specified
#' `timezone`.
#'
#' @param startdate Desired start datetime.
#' @param enddate Desired end datetime.
#' @param timezone Olson timezone used to interpret incoming dates.
#' @param unit Temporal precision used for the returned end-of-range value.
#'   One of `"day"`, `"hour"`, `"min"`, or `"sec"`.
#' @param ceilingStart Logical specifying whether to round `startdate` up to the
#'   next day boundary instead of down.
#' @param ceilingEnd Logical specifying whether to include the entirety of the
#'   final day.
#' @param days Number of days to include when either `startdate` or `enddate`
#'   is omitted.
#'
#' @return
#' Two-element `POSIXct` vector ordered from earliest to latest.
#'
#' @section Default arguments:
#' If either `startdate` or `enddate` is missing, the missing boundary is
#' calculated using `days`.
#'
#' If both are missing, `enddate` defaults to the current day in `timezone`
#' and `startdate` is calculated as `enddate - days`.
#'
#' @section End-of-day units:
#' The returned end time is adjusted to the last representable value within the
#' requested unit:
#'
#' \describe{
#'   \item{`unit = "day"`}{End time is midnight at the start of the final day.}
#'   \item{`unit = "hour"`}{End time is `23:00:00`.}
#'   \item{`unit = "min"`}{End time is `23:59:00`.}
#'   \item{`unit = "sec"`}{End time is `23:59:59`.}
#' }
#'
#' @section POSIXct inputs:
#' When `startdate` or `enddate` are already `POSIXct` values, they are first
#' converted to `timezone` with [lubridate::with_tz()] without changing the
#' represented instant in time.
#'
#' @section Parameter precedence:
#' When parameters conflict, the following rules apply:
#'
#' \enumerate{
#'   \item If both `startdate` and `enddate` are supplied, `days` is ignored.
#'   \item If `startdate` is missing, `ceilingStart` is ignored.
#'   \item If `enddate` is missing, `ceilingEnd` is ignored.
#' }
#'
#' @examples
#' dateRange("2019-01-08", timezone = "UTC")
#'
#' dateRange("2019-01-08", unit = "min", timezone = "UTC")
#'
#' dateRange("2019-01-08", unit = "hour", timezone = "UTC")
#'
#' dateRange("2019-01-08", unit = "day", timezone = "UTC")
#'
#' dateRange("2019-01-08", "2019-01-11", timezone = "UTC")
#'
#' dateRange(
#'   enddate = 20190112,
#'   days = 3,
#'   unit = "day",
#'   timezone = "America/Los_Angeles"
#' )
#'
#' @export
#'
dateRange <- function(
  startdate = NULL,
  enddate = NULL,
  timezone = NULL,
  unit = "sec",
  ceilingStart = FALSE,
  ceilingEnd = FALSE,
  days = 7
) {

  # Validate parameters --------------------------------------------------------

  stopIfNull(timezone)
  stopIfNull(unit)
  ceilingStart <- setIfNull(ceilingStart, FALSE, "logical")
  ceilingEnd <- setIfNull(ceilingEnd, FALSE, "logical")

  if ( !timezone %in% base::OlsonNames() )
    stop(sprintf("'timezone = %s' is not found in OlsonNames()", timezone))

  if ( !is.numeric(days) || length(days) > 1 || days < 1 )
    stop("'days' must be a single positive number")

  if ( !is.null(startdate) && length(startdate) != 1 )
    stop("'startdat'e must be of length one, if specified")

  if ( !is.null(enddate) && length(enddate) != 1 )
    stop("'enddate' must be of length one, if specified")


  # Handle end-of-day unit -----------------------------------------------------

  if ( stringr::str_detect(unit, "^day") ) {
    endUnitAdjust <- lubridate::days(0)
  } else if ( stringr::str_detect(unit, "^hour") ) {
    endUnitAdjust <- lubridate::hours(1)
  } else if ( stringr::str_detect(unit, "^min") ) {
    endUnitAdjust <- lubridate::minutes(1)
  } else if ( stringr::str_detect(unit, "^sec") ) {
    endUnitAdjust <- lubridate::seconds(1)
  } else {
    stop("'unit' must be one of: 'day', 'hour', 'min', 'sec'")
  }

  # Determine start and end times ----------------------------------------------

  # NOTE:  Always assume floor_date for both start- and enddate and take
  # NOTE:  care of ceilingEnd as the very last step

  if ( !is.null(startdate) && !is.null(enddate) ) {

    # ** Both found: use startdate, enddate ------------------------------------

    # Handle parsing and ordering
    timeInput <- timeRange(startdate, enddate, timezone = timezone)

    if ( ceilingStart ) {
      starttime <-
        timeInput[1] %>%
        lubridate::ceiling_date(unit = "day")
    } else {
      starttime <-
        timeInput[1] %>%
        lubridate::floor_date(unit = "day")
    }

    endtime <-
      timeInput[2] %>%
      lubridate::floor_date(unit = "day")

    if ( starttime == endtime ) {
      # Handle single-day ranges
      endtime <- endtime + lubridate::ddays(1)
    } else {
      # Adjust for ceilingEnd
      if ( ceilingEnd ) {
        endtime <- endtime + lubridate::ddays(1)
      }
    }

    endtime <- endtime - endUnitAdjust

  } else if ( !is.null(startdate) && is.null(enddate) ) {

  # ** Missing enddate: use startdate, (startdate + days) ----------------------

    if ( ceilingStart ) {
      starttime <-
        startdate %>%
        parseDatetime(timezone = timezone) %>%
        lubridate::ceiling_date(unit = "day")
    } else {
      starttime <-
        startdate %>%
        parseDatetime(timezone = timezone) %>%
        lubridate::floor_date(unit = "day")
    }

    endtime <- starttime + lubridate::days(days)

    # Ignore "ceilingEnd" parameter

    endtime <- endtime - endUnitAdjust

  } else if ( is.null(startdate) && !is.null(enddate) ) {

    # ** Missing startdate: use (enddate - days), enddate ----------------------

    endtime <-
      enddate %>%
      parseDatetime(timezone = timezone) %>%
      lubridate::floor_date(unit = "day")

    if ( ceilingEnd ) {
      endtime <- endtime + lubridate::ddays(1)
    }

    # Ignore ceilingStart

    starttime <- endtime - lubridate::days(days)

    endtime <- endtime - endUnitAdjust


  } else {

    # ** Both missing: use (now - days), now -----------------------------------

    endtime <-
      lubridate::now(tzone = timezone) %>%
      lubridate::floor_date(unit = "day")

    if ( ceilingEnd ) {
      endtime <- endtime + lubridate::ddays(1)
    }

    # Ignore ceilingStart

    starttime <- endtime - lubridate::days(days)

    endtime <- endtime - endUnitAdjust

  }


  # Return tlim ----------------------------------------------------------------

  return(c(starttime, endtime))

}
