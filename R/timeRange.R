#' @export
#'
#' @title Create a POSIXct time range
#'
#' @param starttime Desired start datetime (ISO 8601).
#' @param endtime Desired end datetime (ISO 8601).
#' @param timezone Olson timezone used to interpret dates.
#'
#' @description
#' Uses incoming parameters to return a pair of \code{POSIXct} times in the
#' proper order.
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
  timezone = NULL
) {

  # Validate parameters --------------------------------------------------------

  if ( is.null(starttime) )
    stop("Required parameter 'starttime' is missing.")

  if ( is.null(endtime) )
    stop("Required parameter 'endtime' is missing.")

  if ( is.null(timezone) )
    stop("Required parameter 'timezone' is missing.")

  if ( !timezone %in% base::OlsonNames() )
    stop(paste0("Timezone '", timezone, "' is not recognized."))

  # * Prepare POSIXct inputs ---------------------------------------------------

  ## NOTE on hadling POSIXct inputs:
  #  When given a POSIXct time `lubridate::parse_date_time()` forces the time
  #  into the timezone given to `lubridate::parse_date_time()`. This alters the
  #  physical instant in time the original POSIXct represents, so we must
  #  properly convert a POSIXct start or end date to the proper timezone before
  #  passing it to `lubridate::parse_date_time()`

  if ( lubridate::is.POSIXct(starttime) )
    starttime <- lubridate::with_tz(starttime, tzone = timezone)

  if ( lubridate::is.POSIXct(endtime) )
    endtime <- lubridate::with_tz(endtime, tzone = timezone)


  # * Parse inputs -------------------------------------------------------------

  orders <- c("Ymd", "YmdH", "YmdHM", "YmdHMS")

  timeInputs <-
    c(starttime, endtime) %>%
    lubridate::parse_date_time(orders = orders, tz = timezone)


  # Order output time limits ---------------------------------------------------

  return(sort(timeInputs))

}
