#' Create a POSIXct date sequence
#'
#' Create a sequence of local-midnight `POSIXct` datetimes in a specified
#' timezone.
#'
#' The returned sequence begins at midnight local time on `startdate` and ends
#' at midnight local time on `enddate`, *i.e.* the *beginning* of `enddate`.
#'
#' The `ceilingEnd` argument addresses ambiguity in date ranges such as
#' `"August 1-8"`. With `ceilingEnd = FALSE` (default), the sequence ends at
#' the beginning of August 8. With `ceilingEnd = TRUE`, the sequence
#' includes the entirety of August 8, ending at the midnight that begins August 9.
#'
#' Input dates are parsed with [parseDatetime()] using the specified
#' `timezone`. Any hour-minute-second information is removed after parsing.
#'
#' @note
#' This function preserves local clock-time midnight boundaries across daylight
#' savings transitions. This differs from `seq.Date(..., by = "day")`, which
#' advances by fixed 24-hour intervals and can drift away from midnight local
#' time during daylight savings changes.
#'
#' @param startdate Desired start datetime.
#' @param enddate Desired end datetime.
#' @param timezone Olson timezone used to interpret incoming dates.
#' @param ceilingEnd Logical specifying whether to include the end of the final
#'   day.
#'
#' @return
#' A vector of `POSIXct` datetimes at local midnight.
#'
#' @section POSIXct inputs:
#' When `startdate` or `enddate` are already `POSIXct` values, they are first
#' converted to `timezone` with [lubridate::with_tz()] without changing the
#' represented instant in time. They are then floored to local midnight.
#'
#' @examples
#' dateSequence(
#'   "2019-11-01",
#'   "2019-11-08",
#'   timezone = "America/Los_Angeles"
#' )
#'
#' dateSequence(
#'   "2019-11-01",
#'   "2019-11-07",
#'   timezone = "America/Los_Angeles",
#'   ceilingEnd = TRUE
#' )
#'
#' # Observe daylight savings handling
#' datetime <- dateSequence(
#'   "2019-11-01",
#'   "2019-11-08",
#'   timezone = "America/Los_Angeles"
#' )
#'
#' datetime
#' lubridate::with_tz(datetime, "UTC")
#'
#' # POSIXct inputs preserve the represented instant before flooring
#' jst <- dateSequence(
#'   20190307,
#'   20190315,
#'   timezone = "Asia/Tokyo"
#' )
#'
#' jst
#'
#' dateSequence(
#'   jst[1],
#'   jst[7],
#'   timezone = "UTC"
#' )
#'
#' @export
#'
dateSequence <- function(
  startdate = NULL,
  enddate = NULL,
  timezone = NULL,
  ceilingEnd = FALSE
) {

  # ----- Validate parameters --------------------------------------------------

  stopIfNull(startdate)
  stopIfNull(enddate)
  stopIfNull(timezone)
  stopIfNull(ceilingEnd)

  if ( !timezone %in% base::OlsonNames() )
    stop(sprintf("'timezone = %s' is not found in OlsonNames()", timezone))

  # ----- Create sequence ------------------------------------------------------

  # NOTE:  Need to interpret date in local time first, then floor, then go to
  # NOTE:  the middle of the day. Otherwise strftime() below will repeat the
  # NOTE:  date on which we switch from daylight savings to standard time.
  start <-
    parseDatetime(startdate, timezone = timezone) %>%
    lubridate::floor_date(unit = "day") + lubridate::dhours(12)

  end <-
    parseDatetime(enddate, timezone = timezone) %>%
    lubridate::floor_date(unit = "day") + lubridate::dhours(12)

  if ( ceilingEnd )
    end <- end + lubridate::ddays(1)

  # NOTE:  seq.Date(..., by = "day") operates by repeatedly adding 24 hours
  # NOTE:  which means that when we switch to/from daylight savings we end up
  # NOTE:  no longer on the midnight local time day boundary. Hence the
  # NOTE:  following workaround

  datetime <-
    seq(start, end, by = "day") %>%
    strftime("%Y%m%d", tz = timezone) %>%
    MazamaCoreUtils::parseDatetime(timezone = timezone)

  # ----- Return ---------------------------------------------------------------

  return(datetime)

}
