#' @title Create a POSIXct date range
#'
#' @description
#' Uses incoming parameters to return a pair of \code{POSIXct} times in the
#' proper order. The first returned time will be midnight of the desired
#' starting date. The second returned time will represent the "end of the day"
#' of the requested or calcualted \code{enddate}.
#'
#' The required \code{timezone} parameter must be one of those found in
#' \code{\link[base]{OlsonNames}}.
#'
#' Dates can be anything that is understood by
#' \code{lubrdiate::parse_date_time()} using the \code{Ymd[HMS]} orders. This
#' includes:
#'
#' \itemize{
#'   \item{\code{"YYYYmmdd"}}
#'   \item{\code{"YYYYmmddHHMMSS"}}
#'   \item{\code{"YYYY-mm-dd"}}
#'   \item{\code{"YYYY-mm-dd H"}}
#'   \item{\code{"YYYY-mm-dd H:M"}}
#'   \item{\code{"YYYY-mm-dd H:M:S"}}
#' }
#'
#' @param startdate Desired start datetime (ISO 8601).
#' @param enddate Desired end datetime (ISO 8601).
#' @param timezone Olson timezone used to interpret dates (required).
#' @param days Number of days of data to include.
#' @param unit Units used to determine time at end-of-day.
#'
#' @return A vector of two \code{POSIXct}s.
#'
#' @section Default Arguments:
#' In the case when either \code{startdate} or \code{enddate} is missing, it is
#' created from the non-missing values plus/minus \code{days}. If both
#' \code{startdate} and \code{enddate} are misssing, \code{enddate} is set to
#' \code{\link[lubridate]{now}} (with the given \code{timezone}), and then
#' \code{startdate} is calculated using \code{enddate - days}.
#'
#' @section End-of-Day Units:
#' The second of the returned \code{POSIXct}s will end one \code{unit} before
#' the end of the specified \code{enddate}. Acceptable units are \code{"day",
#' "hour", "min", "sec"}.
#'
#' The aim is to quickly calculate full-day date ranges for time series whose
#' values are binned at different units. Thus, if \code{unit = "min"}, the
#' returned value associated with \code{enddate} will always be at 23:59:00
#' in the requested time zone.
#'
#' Because the returned values always satisfy full-day date ranges, specifying
#' \code{unit = "day"} will cause the returned \code{enddate} to be associated
#' with 00:00:00 of the following day.
#'
#' @section POSIXct inputs:
#' When \code{startdate} or \code{enddate} are already \code{POSIXct} values,
#' they are converted to the timezone specified by \code{timezone} without
#' altering the physical instant in time the input represents. This is different
#' from the behavior of \code{\link[lubridate]{parse_date_time}} (which powers
#' this function), which will force \code{POSIXct} inputs into a new timezone,
#' altering the physical moment of time the input represents.
#'
#' @export
#'
#' @examples
#' dateRange("2019-01-08", timezone = "UTC")
#' dateRange("2019-01-08", unit = "min", timezone = "UTC")
#' dateRange("2019-01-08", unit = "hour", timezone = "UTC")
#' dateRange("2019-01-08", unit = "day", timezone = "UTC")
#' dateRange("2019-01-08", "2019-01-11", timezone = "UTC")
#' dateRange(enddate = 20190112, days = 3,
#'           unit = "day", timezone = "America/Los_Angeles")
dateRange <- function(
  startdate = NULL,
  enddate = NULL,
  timezone = NULL,
  days = 7,
  unit = "sec"
) {

  # Validate parameters --------------------------------------------------------

  if ( is.null(timezone) )
    stop("Required parameter 'timezone' is missing.")

  if ( !timezone %in% base::OlsonNames() )
    stop(paste0("Timezone '", timezone, "' is not recognized."))

  if ( !is.numeric(days) || length(days) > 1 || days < 1 )
    stop("`days` must be a single positive number.")

  if ( !is.null(startdate) && length(startdate) != 1 )
    stop("startdate must be of length one, if specified.")

  if ( !is.null(enddate) && length(enddate) != 1 )
    stop("enddate must be of length one, if specified.")


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
    stop("'unit' must be one of: 'day', 'hour', 'min', 'sec'.")
  }


  # Determine start and end times ----------------------------------------------

  # * Prepare POSIXct inputs ---------------------------------------------------

  ## NOTE on hadling POSIXct inputs:
  #  When given a POSIXct time `lubridate::parse_date_time()` forces the time
  #  into the timezone given to `lubridate::parse_date_time()`. This alters the
  #  physical instant in time the original POSIXct represents, so we must
  #  properly convert a POSIXct start or end date to the proper timezone before
  #  passing it to `lubridate::parse_date_time()`

  if ( lubridate::is.POSIXct(startdate) )
    startdate <- lubridate::with_tz(startdate, tzone = timezone)

  if ( lubridate::is.POSIXct(enddate) )
    enddate <- lubridate::with_tz(enddate, tzone = timezone)


  # * Parse inputs -------------------------------------------------------------

  orders <- c("Ymd", "YmdH", "YmdHM", "YmdHMS")

  if ( !is.null(startdate) && !is.null(enddate) ) {

    # ** Both found: use startdate, enddate ------------------------------------

    # handle ordering
    timeInputs <- sort(c(
      lubridate::parse_date_time(startdate, orders = orders, tz = timezone),
      lubridate::parse_date_time(enddate, orders = orders, tz = timezone)
    ))

    starttime <- lubridate::floor_date(timeInputs[1], unit = "day")

    endtime <-
      timeInputs[2] %>%
      lubridate::ceiling_date(unit = "day") %>%
      `-`(endUnitAdjust)


  } else if ( !is.null(startdate) && is.null(enddate) ) {

  # ** Missing enddate: use startdate, (startdate + days) ----------------------

    starttime <-
      startdate %>%
      lubridate::parse_date_time(orders = orders, tz = timezone) %>%
      lubridate::floor_date(unit = "day")

    endtime <- starttime + lubridate::days(days) - endUnitAdjust


  } else if ( is.null(startdate) && !is.null(enddate) ) {

    # ** Missing startdate: use (enddate - days), enddate ----------------------

    ## NOTE:
    #  Don't account for end unit adjustments until after calculating the start
    #  time from the end time, in order to make the math for subtracting the
    #  correct amount of time from the end time simpler.

    endtime <-
      enddate %>%
      lubridate::parse_date_time(orders = orders, tz = timezone) %>%
      lubridate::ceiling_date(unit = "day")

    starttime <- endtime - lubridate::days(days)
    endtime <- endtime - endUnitAdjust


  } else {

    # ** Both missing: use (now - days), now -----------------------------------

    endtime <-
      lubridate::now(tzone = timezone) %>%
      lubridate::ceiling_date(unit = "day")

    starttime <- endtime - lubridate::days(days)
    endtime <- endtime - endUnitAdjust

  }


  # Return tlim ----------------------------------------------------------------

  return(c(starttime, endtime))

}
