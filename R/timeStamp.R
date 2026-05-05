#' Create character timestamps
#'
#' Convert datetimes to compact character timestamps suitable for file names,
#' identifiers, labels, and other reproducible text output.
#'
#' Input values are converted with [parseDatetime()] using the required
#' `timezone` argument. When `datetime = NULL`, the current UTC time is used
#' and `timezone` defaults to `"UTC"`.
#'
#' The `unit` argument controls the precision of the output timestamp. The
#' `style` argument controls the output format.
#'
#' Supported `unit` values are:
#'
#' \preformatted{
#' "year"
#' "month"
#' "day"
#' "hour"
#' "min"
#' "sec"
#' "msec"
#' }
#'
#' Supported `style` values are:
#'
#' \preformatted{
#' "ymdhms"   compact calendar time
#' "ymdThms"  compact calendar time with "T" separator
#' "julian"   year and Julian day
#' "clock"    ISO-like clock time
#' }
#'
#' For `style = "julian"` and `unit = "month"`, the timestamp uses the Julian
#' day associated with the beginning of the month.
#'
#' @param datetime Vector of character, integer, or `POSIXct` datetimes.
#' @param timezone Olson timezone used to interpret incoming datetimes.
#' @param unit Temporal precision of the generated timestamp.
#' @param style Output timestamp style.
#'
#' @return
#' Character vector of timestamps.
#'
#' @inheritSection dateRange POSIXct inputs
#'
#' @examples
#' datetime <- parseDatetime("2019-01-08 12:30:15", timezone = "UTC")
#'
#' timeStamp()
#' timeStamp(datetime, "UTC", unit = "year")
#' timeStamp(datetime, "UTC", unit = "month")
#' timeStamp(datetime, "UTC", unit = "month", style = "julian")
#' timeStamp(datetime, "UTC", unit = "day")
#' timeStamp(datetime, "UTC", unit = "day", style = "julian")
#' timeStamp(datetime, "UTC", unit = "hour")
#' timeStamp(datetime, "UTC", unit = "min")
#' timeStamp(datetime, "UTC", unit = "sec")
#' timeStamp(datetime, "UTC", unit = "sec", style = "ymdThms")
#' timeStamp(datetime, "UTC", unit = "sec", style = "julian")
#' timeStamp(datetime, "UTC", unit = "sec", style = "clock")
#' timeStamp(datetime, "America/Los_Angeles", unit = "sec", style = "clock")
#' timeStamp(datetime, "America/Los_Angeles", unit = "msec", style = "clock")
#'
#' @export
#'
timeStamp <- function(
  datetime = NULL,
  timezone = NULL,
  unit = "sec",
  style = "ymdhms"
) {

  # ----- Validate parameters --------------------------------------------------

  # Common use case
  if ( is.null(datetime) ) {
    datetime <- lubridate::now(tzone = "UTC")
    if ( is.null(timezone) ) timezone <- "UTC"
  }

  stopIfNull(datetime)
  stopIfNull(timezone)
  stopIfNull(unit)
  stopIfNull(style)

  if ( !timezone %in% base::OlsonNames() )
    stop(sprintf("'timezone = %s' is not found in OlsonNames()", timezone))

  if ( !unit %in% c("year", "month", "day", "hour", "min", "sec", "msec") )
    stop(sprintf("'unit = %s' is not recognized", unit))

  if ( !style %in% c("ymdhms", "ymdThms", "julian", "clock") )
    stop(sprintf("'style = %s' is not recognized", style))

  # ----- Format datetimes -----------------------------------------------------

  # Guarantee conversion to POSIXct
  datetime <- parseDatetime(datetime, timezone = timezone)

  if ( unit == "year" ) {
    format <- "%Y"

  } else if ( unit == "month" ) {
    if ( style == "ymdhms" )
      format <- "%Y%m"
    if ( style == "ymdThms" )
      format <- "%Y%m"
    if ( style == "julian" ) {
      datetime <- lubridate::floor_date(datetime, unit = "month")
      format <- "%Y%j"
    }
    if ( style == "clock" )
      format <- "%Y-%m"

  } else if ( unit == "day" ) {
    if ( style == "ymdhms" )
      format <- "%Y%m%d"
    if ( style == "ymdThms" )
      format <- "%Y%m%d"
    if ( style == "julian" )
      format <- "%Y%j"
    if ( style == "clock" )
      format <- "%Y-%m-%d"

  } else if ( unit == "hour" ) {
    if ( style == "ymdhms" )
      format <- "%Y%m%d%H"
    if ( style == "ymdThms" )
      format <- "%Y%m%dT%H"
    if ( style == "julian" )
      format <- "%Y%j%H"
    if ( style == "clock" )
      format <- "%Y-%m-%dT%H"

  } else if ( unit == "min" ) {
    if ( style == "ymdhms" )
      format <- "%Y%m%d%H%M"
    if ( style == "ymdThms" )
      format <- "%Y%m%dT%H%M"
    if ( style == "julian" )
      format <- "%Y%j%H%M"
    if ( style == "clock" )
      format <- "%Y-%m-%dT%H:%M"

  } else if ( unit == "sec" ) {
    if ( style == "ymdhms" )
      format <- "%Y%m%d%H%M%S"
    if ( style == "ymdThms" )
      format <- "%Y%m%dT%H%M%S"
    if ( style == "julian" )
      format <- "%Y%j%H%M%S"
    if ( style == "clock" )
      format <- "%Y-%m-%dT%H:%M:%S"

  } else if ( unit == "msec" ) {
    if ( style == "ymdhms" )
      format <- "%Y%m%d%H%M%OS3"
    if ( style == "ymdThms" )
      format <- "%Y%m%dT%H%M%OS3"
    if ( style == "julian" )
      format <- "%Y%j%H%M%OS3"
    if ( style == "clock" )
      format <- "%Y-%m-%dT%H:%M:%OS3"

  }

  timeStamp <- strftime(datetime, format = format, tz = timezone)

  # ----- Return ---------------------------------------------------------------

  return(timeStamp)

}
