#' @export
#'
#' @title Create a POSIXct date range
#'
#' @param startdate Desired start datetime (ISO 8601).
#' @param enddate Desired end datetime (ISO 8601).
#' @param days Number of days of data to include.
#' @param timezone Olson timezone used to interpret dates.
#' @param unit Units used to determine time at end-of-day.
#'
#' @description Uses incoming parameters to return a pair of \code{POSIXct}
#' times in the proper order. The first returned time will be midnight of
#' the desired starting date. The second returned time will represent the
#' "end of the day" of the requested or calcualted \code{enddate}.
#'
#' Dates can be anything that is understood by
#' \code{lubrdiate::parse_date_time()} including either of the following
#' recommended formats:
#'
#' \itemize{
#' \item{\code{"YYYYmmdd"}}
#' \item{\code{"YYYY-mm-dd"}}
#' }
#'
#' @note The second of the returned \code{POSIXct}s will end one \code{unit}
#' before the specified \code{enddate}. Acceptable units are
#' \code{"day", "hour", "min", "sec"}.
#'
#' The aim is to quickly calculate full-day date ranges for time series whose
#' values are binned at different units. Thus, if \code{unit = "min"}, the
#' returned value associated with \code{enddate} will always be at 23:59:00
#' in the requested time zone.
#'
#' @note Because the returned values always satisfy full-day date ranges,
#' specifying \code{unit = "day"} will cause the returned \code{enddate} to
#' be associated with 00:00:00 of the following day.
#'
#' @return A vector of two \code{POSIXct}s.
#'
#' @examples
#' dateRange("2019-01-08")
#' dateRange("2019-01-08", unit = "min")
#' dateRange("2019-01-08", unit = "hour")
#' dateRange("2019-01-08", unit = "day")
#' dateRange("2019-01-08", "2019-01-11")
#' dateRange(enddate = 20190112, days = 3,
#'           unit = "day", timezone = "America/Los_Angeles")

dateRange <- function(
  startdate = NULL,
  enddate = NULL,
  days = 7,
  timezone = "UTC",
  unit = "sec"
) {

  # ----- Determine starttime and endtime --------------------------------------

  if ( stringr::str_detect(unit, "^day") ) {
    daySecs <- 60 * 60 * 24
  } else if ( stringr::str_detect(unit, "^hour") ) {
    daySecs <- 60 * 60 * 24 - 3600
  } else if ( stringr::str_detect(unit, "^min") ){
    daySecs <- 60 * 60 * 24 - 60
  } else if ( stringr::str_detect(unit, "^sec") ) {
    daySecs <- 60 * 60 * 24 - 1
  }

  orders <- c("Ymd","YmdH","YmdHM","YmdHMS")

  if ( !is.null(startdate) && !is.null(enddate) ) {

    # Both found:  use startdate, enddate
    endtime <-
      enddate %>%
      lubridate::parse_date_time(orders = orders, tz = timezone) %>%
      lubridate::floor_date(unit = "day") +
      lubridate::dseconds(daySecs)

    starttime <-
      startdate %>%
      lubridate::parse_date_time(orders = orders, tz = timezone) %>%
      lubridate::floor_date(unit = "day")

  } else if ( is.null(startdate) && !is.null(enddate) ) {

    # Missing startdate:  use (enddate - days), enddate
    endtime <-
      enddate %>%
      lubridate::parse_date_time(orders = orders, tz = timezone) %>%
      lubridate::floor_date(unit = "day") +
      lubridate::dseconds(daySecs)               # end of day

    # Special case for "day" because enddate moves to 00:00:00 of next day
    if ( unit == "day" ) {
      starttime <- endtime - lubridate::ddays(days)
    } else {
      starttime <-
        endtime %>%
        lubridate::floor_date(unit = "day") -      # beginning of day
        lubridate::ddays(days-1)                   # any extra days
    }

  } else if ( !is.null(startdate) && is.null(enddate) ) {

    # Missing enddate:  use startdate, (startdate + days)
    starttime <-
      startdate %>%
      lubridate::parse_date_time(orders = orders, tz = timezone) %>%
      lubridate::floor_date(unit = "day")

    endtime <-
      starttime %>%
      lubridate::floor_date(unit = "day") +
      lubridate::dseconds(daySecs) +             # end of day
      lubridate::ddays(days-1)                   # any extra days

  } else {

    # Both missing:  use (now - days), now
    endtime <-
      lubridate::now(timezone) %>%
      lubridate::floor_date(unit = "day" )

    starttime <-
      endtime %>%
      lubridate::floor_date(unit = "day") -
      lubridate::ddays(days)

  }

  # Define tlim
  if ( starttime < endtime ) {
    tlim <- c(starttime,endtime)
  } else {
    # just in case
    tlim <- c(endtime,starttime)
  }

  return(tlim)

}
