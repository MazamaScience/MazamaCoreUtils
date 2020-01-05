#' @title Create a POSIXct date range
#'
#' @description
#' Uses incoming parameters to return a pair of \code{POSIXct} times in the
#' proper order. The first returned time will be midnight of the desired
#' starting date. The second returned time will represent the "end of the day"
#' of the requested or calculated \code{enddate} boundary.
#'
#' Note that the returned end date will be one \code{unit} prior to the start
#' of the requested \code{enddate} unless \code{ceilingEnd = TRUE} in
#' which case the entire \code{enddate} will be included up to the last
#' \code{unit}.
#'
#' The \code{ceilingEnd} argument addresses the ambiguity of a phrase like:
#' "August 1-8". With \code{ceilingEnd = FALSE} (default) this pharse means
#' "through the beginning of Aug 8". With \code{ceilingEnd = TRUE} it means
#' "through the end of Aug 8".
#'
#' So, to get 24 hours of data staring on Jan 01, 2019 you would specify:
#'
#' \preformatted{
#' > MazamaCoreUtils::dateRange(20190101, 20190102, timezone = "UTC")
#' [1] "2019-01-01 00:00:00 UTC" "2019-01-01 23:59:59 UTC"
#' }
#'
#' or
#'
#' \preformatted{
#' > MazamaCoreUtils::dateRange(20190101, 20190101,
#'                              timezone = "UTC", ceilingEnd = TRUE)
#' [1] "2019-01-01 00:00:00 UTC" "2019-01-01 23:59:59 UTC"
#' }
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
#' @param unit Units used to determine time at end-of-day.
#' @param ceilingStart Logical instruction to apply
#'   \code{\link[lubridate]{ceiling_date}} to the \code{startdate} rather than
#'   \code{\link[lubridate]{floor_date}}
#' @param ceilingEnd Logical instruction to apply
#'   \code{\link[lubridate]{ceiling_date}} to the \code{enddate} rather than
#'   \code{\link[lubridate]{floor_date}}
#' @param days Number of days of data to include.
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
#' the specified \code{enddate}. Acceptable units are \code{"day",
#' "hour", "min", "sec"}.
#'
#' The aim is to quickly calculate full-day date ranges for time series whose
#' values are binned at different units. Thus, if \code{unit = "min"}, the
#' returned value associated with \code{enddate} will always be at 23:59:00
#' in the requested time zone.
#'
#' @section POSIXct inputs:
#' When \code{startdate} or \code{enddate} are already \code{POSIXct} values,
#' they are converted to the timezone specified by \code{timezone} without
#' altering the physical instant in time the input represents. This is different
#' from the behavior of \code{\link[lubridate]{parse_date_time}} (which powers
#' this function), which will force \code{POSIXct} inputs into a new timezone,
#' altering the physical moment of time the input represents.
#'
#' @section Parameter precedence:
#' It is possible to supply input paramters that are in conflict. For example:
#'
#' \code{dateRange("2019-01-01", "2019-01-08", days = 3, timezone = "UTC")}
#'
#' The \code{startdate} and \code{enddate} parameters would imply a 7-day range
#' which is in conflict with \code{days = 3}. The following rules resolve
#' conflicts of this nature:
#'
#' \enumerate{
#' \item{When \code{startdate} and \code{enddate} are both specified, the
#' \code{days} parameter is ignored.}
#' \item{When \code{startdate} is missing, \code{ceilingStart} is ignored and
#' the first returned time will depend on the combination of \code{enddate},
#' \code{days} and \code{ceilingEnd}.}
#' \item{When \code{enddate} is missing, \code{ceilingEnd} is ignored and the
#' second returned time depends on \code{ceilingStart} and \code{days}.}
#' }
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
  unit = "sec",
  ceilingStart = FALSE,
  ceilingEnd = FALSE,
  days = 7
) {

  # Validate parameters --------------------------------------------------------

  stopIfNull(timezone)
  stopIfNull(unit)
  stopIfNull(ceilingEnd)

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

    # Handle single-day ranges
    if ( starttime == endtime ) {
      endtime <- endtime + lubridate::ddays(1)
    }

    # Ignore "days" parameter

    # Adjust for ceilingEnd
    if ( ceilingEnd ) {
      endtime <- endtime + lubridate::ddays(1)
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
