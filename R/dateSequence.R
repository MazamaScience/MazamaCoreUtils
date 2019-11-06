#' @title Create a POSIXct date sequence
#'
#' @description
#' Uses incoming parameters to return a seqeunce of \code{POSIXct} times at
#' local midnight in the specified \code{timezone}. The first returned time will
#' be midnight of the requested \code{startdate}. The final returned time will
#' be midnight (\emph{at the beginning}) of the requested \code{enddate}.
#'
#' The \code{ceilingEnd} argument addresses the ambiguity of a phrase like:
#' "August 1-8". With \code{ceilingEnd = FALSE} (default) this pharse means
#' "through the beginning of Aug 8". With \code{ceilingEnd = TRUE} it means
#' "through the end of Aug 8".
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
#' All hour-minute-second information is removed after parsing.
#'
#' @note The main utility of this function is that it respects "clock time" and
#' returns times associated with midnight regardless of daylight savings. This
#' is in contrast to `seq.Date(from, to, by = "day")` which creates a sequence
#' of datetimes always separated by 24 hours.
#'
#' @param startdate Desired start datetime (ISO 8601).
#' @param enddate Desired end datetime (ISO 8601).
#' @param timezone Olson timezone used to interpret dates (required).
#' @param ceilingEnd Logical instruction to apply
#'   \code{\link[lubridate]{ceiling_date}} to the \code{enddate} rather than
#'   \code{\link[lubridate]{floor_date}}
#'
#' @return A vector of \code{POSIXct}s at midnight local time.
#'
#' @section POSIXct inputs:
#' When \code{startdate} or \code{enddate} are already \code{POSIXct} values,
#' they are converted to the timezone specified by \code{timezone} without
#' altering the physical instant in time the input represents. Only after
#' conversion are they floored to midnight local time
#'
#' @export
#'
#' @examples
#' dateSequence("2019-11-01", "2019-11-08", timezone = "America/Los_Angeles")
#' dateSequence("2019-11-01", "2019-11-07", timezone = "America/Los_Angeles",
#'              ceilingEnd = TRUE)
#'
#' # Observe the handling of daylight savings
#' datetime <- dateSequence("2019-11-01", "2019-11-08",
#'                          timezone = "America/Los_Angeles")
#'
#' datetime
#' lubridate::with_tz(datetime, "UTC")
#'
#' # Passing in POSIXct values preserves the instant in time before flooring --
#' #   midnight Tokyo time is the day before in UTC
#' jst <- dateSequence(20190307, 20190315, timezone = "Asia/Tokyo")
#' jst
#' dateSequence(jst[1], jst[7], timezone = "UTC")
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
    stop(paste0("Timezone '", timezone, "' is not recognized."))

  # ----- Create sequence ------------------------------------------------------

  # NOTE:  Need to interpret date in local time first, then floor, then go to
  # NOTE:  the middle of the day. Otherwise strftime() below will repeat the
  # NOTE:  date on which switch from daylight savings to standard time.
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
