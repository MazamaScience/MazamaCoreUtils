#' @title Parse datetime strings
#'
#' @description
#' Transforms numeric and string representations of Ymd[HMS] datetimes to
#' \code{POSIXct} format.
#'
#' Y, Ym, Ymd, YmdH, YmdHM, and YmdHMS formats are understood, where:
#'
#' \describe{
#'   \item{Y}{four digit year}
#'   \item{m}{month number (1-12, 01-12) or english name month (October, oct.)}
#'   \item{d}{day number of the month (0-31 or 01-31)}
#'   \item{H}{hour number (0-24 or 00-24)}
#'   \item{M}{minute number (0-59 or 00-59)}
#'   \item{S}{second number (0-61 or 00-61)}
#' }
#'
#' This allows for mixed inputs. For example, 20181012130900,
#' "2018-10-12-13-09-00", and "2018 Oct. 12 13:09:00" will all be converted to
#' the same \code{POSIXct} datetime. The incoming datetime vector does not need
#' to have a homogeneous format either -- "20181012" and "2018-10-12 13:09" can
#' exist in the same vector without issue. All incoming datetimes will be
#' interpreted in the specified timezone.
#'
#' If \code{datetime} is a \code{POSIXct} it will be returned unmodified, and
#' formats not recognized will be returned as \code{NA}.
#'
#' @param datetime Vector of character or integer datetimes in Ymd[HMS] format
#'   (or POSIXct).
#' @param timezone Olson timezone used to interpret dates (required).
#' @param expectAll Logical value determining if the function should fail if
#'   any elements fail to parse (default \code{FALSE}).
#' @param isJulian Logical value determining whether \code{datetime} should be
#' interpreted as a Julian date with day of year as a decimal number.
#' @param quiet Logical value passed on to \code{lubridate::parse_date_time} to
#'   optionally suppress warning messages.
#'
#' @return A vector of POSIXct datetimes.
#'
#' @section Mazama Science Conventions:
#' Within Mazama Science package, datetimes not in \code{POSIXct} format are
#' often represented as decimal values with no separation (ex: 20181012,
#' 20181012130900), either as numerics or strings.
#'
#' @section Implementation:
#' \code{parseDatetime} is essentially a wrapper around
#' \code{\link[lubridate]{parse_date_time}}, handling which formats we want to
#' account for.
#'
#' @seealso \code{\link[lubridate]{parse_date_time}} for implementation details.
#'
#' @export
#'
#' @examples
#' # All y[md-hms] formats are accepted
#' parseDatetime(2018, timezone = "America/Los_Angeles")
#' parseDatetime(201808, timezone = "America/Los_Angeles")
#' parseDatetime(20180807, timezone = "America/Los_Angeles")
#' parseDatetime(2018080718, timezone = "America/Los_Angeles")
#' parseDatetime(201808071812, timezone = "America/Los_Angeles")
#' parseDatetime(20180807181215, timezone = "America/Los_Angeles")
#' parseDatetime("2018-08-07 18:12:15", timezone = "America/Los_Angeles")
#'
#' # Julian days are accepeted
#' parseDatetime(2018219181215, timezone = "America/Los_Angeles",
#'               isJulian = TRUE)
#'
#' # Vector dates are accepted and daylight savings is respected
#' parseDatetime(
#'   c("2018-10-24 12:00", "2018-10-31 12:00",
#'     "2018-11-07 12:00", "2018-11-08 12:00"),
#'   timezone = "America/New_York"
#' )
#'
#' badInput <- c("20181013", NA, "20181015", "181016", "10172018")
#'
#' # Return a vector with \code{NA} for dates that could not be parsed
#' parseDatetime(badInput, timezone = "UTC", expectAll = FALSE)
#'
#' \dontrun{
#' # Fail if any dates cannot be parsed
#' parseDatetime(badInput, timezone = "UTC", expectAll = TRUE)
#' }
#'

parseDatetime <- function(
  datetime = NULL,
  timezone = NULL,
  expectAll = FALSE,
  isJulian = FALSE,
  quiet = TRUE
) {

  # Validate parameters --------------------------------------------------------

  stopIfNull(datetime)
  stopIfNull(timezone)
  stopIfNull(expectAll)
  stopIfNull(isJulian)
  stopIfNull(quiet)

  if ( !is.character(timezone) || length(timezone) > 1 )
    stop(paste0("argument 'timezone' must be a character string of length one"))

  if ( !timezone %in% base::OlsonNames() )
    stop(paste0("timezone '", timezone, "' is not recognized."))

  if ( !is.logical(expectAll) || length(expectAll) != 1 )
    stop("argument 'expectAll' must be a logical value of length one.")

  if ( !is.logical(isJulian) || length(isJulian) != 1 )
    stop("argument 'isJulian' must be a logical value of length one.")

  # Return early if already POSIXct -----------------------------------------

  if (lubridate::is.POSIXct(datetime))
    return(lubridate::with_tz(datetime, tzone = timezone))

  # Parse datetimes ---------------------------------------------------------

  if ( isJulian ) {

    # NOTE:  Julian date strings created by NASA satellite products often
    # NOTE:  include a digit for fractional seconds but no "." decimal marker.
    # NOTE:  We test for and fix that here.

    # Convert possible integers to character and separate fractional seconds
    datetime <- as.character(datetime)
    wholePart <- stringr::str_sub(datetime, 1, 13)
    fractionalPart <- stringr::str_sub(datetime, 14, -1)

    # Corrected date strings
    datetime <-
      paste0(wholePart, ".", fractionalPart) %>%
      stringr::str_replace("\\.$", "")

    orders <- c("Y", "Yj", "YjH", "YjHM", "YjHMS")
    parsedDatetime <- lubridate::parse_date_time(datetime,
                                                 orders,
                                                 tz = timezone,
                                                 quiet = quiet)

  } else {

    orders <- c("Y", "Ym", "Ymd", "YmdH", "YmdHM", "YmdHMS")
    parsedDatetime <- lubridate::parse_date_time(datetime,
                                                 orders,
                                                 tz = timezone,
                                                 quiet = quiet)

  }

  # Handle results ----------------------------------------------------------

  if ( all(is.na(parsedDatetime)) ) {
    stop("No datetimes could be parsed.")
  }

  if ( expectAll ) {

    ## NAs that appear in the parsed datetimes and not in the original datetimes
    #  are datetimes that failed to parse (ie, not originally NA).
    failedIndices <- setdiff(
      which(is.na(parsedDatetime)),
      which(is.na(datetime))
    )

    ## If there already exist NAs in datetime, we don't want to accidently fail
    #  if all non-NA values were parsed
    if (length(failedIndices) == 1) {
      stop(paste0(
        "1 datetime failed to parse (at index: ", failedIndices, ")."
      ))

    # account for differences in plural spellings
    } else if (length(failedIndices) > 1) {
      stop(paste0(
        length(failedIndices), " datetimes failed to parse (at indices: ",
        paste0(failedIndices, collapse = ", "), ")."
      ))
    }
  }

  return(parsedDatetime)

}
