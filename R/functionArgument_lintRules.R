#' @export
#' @docType data
#' @name timezoneLintRules
#' @title Rules for timezone linting.
#' @format A list of function = argument pairs.
#' @description This set of rules is for use with the
#' \code{lintFunctionArgs_~()}. functions. It includes all time-related
#' functions from the \pkg{base} and \pkg{lubridate} packages that are involved
#' with parsing or formatting datetimes and helps check whether the appropriate
#' timezone arguments are being explicitly used.
#'
timezoneLintRules <- list(
  # base functions
  "as.Date" = "tz",
  "as.POSIXct" = "tz",
  "as.POSIXlt" = "tz",
  "ISOdate" = "tz",
  "ISOdatetime" = "tz",
  "strftime" = "tz",
  "strptime" = "tz",
  "Sys.Date" = "DEPRECATED", # Please don't use this function!
  "Sys.time" = "DEPRECATED", # Please don't use this function!
  # lubridate functions
  "as_datetime" = "tz",
  "date_decimal" = "tz",
  "fast_strptime" = "tz",
  "force_tz" = "tzone",
  "force_tzs" = "tzone_out",
  "interval" = "tzone",
  "local_time" = "tz",
  "make_datetime" = "tz",
  "now" = "tzone",
  "parse_date_time" = "tz",
  "parse_date_time2" = "tz",
  "today" = "tzone",
  "with_tz" = "tzone",
  "ymd" = "tz",
  "ymd_h" = "tz",
  "ymd_hm" = "tz",
  "ymd_hms" = "tz",
  # MazamaCoreUtils functions
  "dateRange" = "timezone",
  "timeRange" = "timezone",
  "parseDatetime" = "timezone"
)
