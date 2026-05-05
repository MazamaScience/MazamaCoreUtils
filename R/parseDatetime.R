#' Parse datetimes
#'
#' Convert character, numeric, integer, or `POSIXct` datetimes to `POSIXct`.
#'
#' This function accepts a variety of compact date/time formats commonly used in
#' Mazama Science packages, including `Y`, `Ym`, `Ymd`, `YmdH`, `YmdHM`, and
#' `YmdHMS`. Inputs may be mixed within the same vector.
#'
#' Examples of equivalent inputs include:
#'
#' \preformatted{
#' 20181012130900
#' "2018-10-12-13-09-00"
#' "2018 Oct. 12 13:09:00"
#' }
#'
#' All incoming datetimes are interpreted in the specified `timezone`. If
#' `datetime` is already `POSIXct`, it is converted to the requested timezone
#' with [lubridate::with_tz()].
#'
#' If a character datetime includes signed offset information, such as
#' `"-07:00"`, that offset is used by [lubridate::parse_date_time()] when
#' determining the equivalent instant.
#'
#' @param datetime Vector of character, numeric, integer, or `POSIXct`
#'   datetimes.
#' @param timezone Olson timezone used to interpret incoming datetimes.
#' @param expectAll Logical value specifying whether to stop if any non-missing
#'   input values fail to parse.
#' @param isJulian Logical value specifying whether `datetime` should be
#'   interpreted as a Julian date using day-of-year notation.
#' @param quiet Logical value passed to [lubridate::parse_date_time()] to
#'   suppress parsing warnings.
#'
#' @return
#' A `POSIXct` vector.
#'
#' @section Mazama Science conventions:
#' Within Mazama Science packages, datetimes not already in `POSIXct` format are
#' often represented as compact decimal values with no separators, such as
#' `20181012` or `20181012130900`, either as numbers or strings.
#'
#' @section Implementation:
#' `parseDatetime()` is a wrapper around [lubridate::parse_date_time()] that
#' defines the datetime formats supported by MazamaCoreUtils.
#'
#' @seealso
#' [lubridate::parse_date_time()]
#'
#' @examples
#' # All Y[mdHMS] formats are accepted
#' parseDatetime(2018, timezone = "America/Los_Angeles")
#' parseDatetime(201808, timezone = "America/Los_Angeles")
#' parseDatetime(20180807, timezone = "America/Los_Angeles")
#' parseDatetime(2018080718, timezone = "America/Los_Angeles")
#' parseDatetime(201808071812, timezone = "America/Los_Angeles")
#' parseDatetime(20180807181215, timezone = "America/Los_Angeles")
#'
#' parseDatetime("2018-08-07 18:12:15", timezone = "America/Los_Angeles")
#' parseDatetime("2018-08-07 18:12:15-07:00", timezone = "UTC")
#'
#' # Julian days are accepted
#' parseDatetime(
#'   2018219181215,
#'   timezone = "America/Los_Angeles",
#'   isJulian = TRUE
#' )
#'
#' # Mixed vector inputs are accepted
#' parseDatetime(
#'   c("2018-10-24 12:00", "201810311200", "2018-11-07 12:00"),
#'   timezone = "America/New_York"
#' )
#'
#' badInput <- c("20181013", NA, "20181015", "181016", "10172018")
#'
#' # Return NA for dates that cannot be parsed
#' parseDatetime(badInput, timezone = "UTC", expectAll = FALSE)
#'
#' \dontrun{
#' # Fail if any non-missing dates cannot be parsed
#' parseDatetime(badInput, timezone = "UTC", expectAll = TRUE)
#' }
#'
#' @export
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
    stop(sprintf("'timezone = %s' is not found in OlsonNames()", timezone))

  if ( !is.logical(expectAll) || length(expectAll) != 1 )
    stop("argument 'expectAll' must be a logical value of length one")

  if ( !is.logical(isJulian) || length(isJulian) != 1 )
    stop("argument 'isJulian' must be a logical value of length one")

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

    orders <- c("Y", "Ym", "Ymd", "YmdH", "YmdHM", "YmdHMS", "YmdHz", "YmdHMz", "YmdHMSz")
    parsedDatetime <- lubridate::parse_date_time(datetime,
                                                 orders,
                                                 tz = timezone,
                                                 quiet = quiet)

  }

  # Handle results ----------------------------------------------------------

  if ( all(is.na(parsedDatetime)) ) {
    stop("no datetimes could be parsed")
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
        "1 datetime failed to parse (at index: ", failedIndices, ")"
      ))

    # account for differences in plural spellings
    } else if (length(failedIndices) > 1) {
      stop(paste0(
        length(failedIndices), " datetimes failed to parse (at indices: ",
        paste0(failedIndices, collapse = ", "), ")"
      ))
    }
  }

  return(parsedDatetime)

}
