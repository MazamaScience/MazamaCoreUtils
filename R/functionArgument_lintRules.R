#' Timezone linting rules
#'
#' Rules used by [lintFunctionArgs_file()] and [lintFunctionArgs_dir()] to find
#' date/time function calls that should explicitly specify timezone arguments.
#'
#' Each list name is a function to check. Each value is the required named
#' timezone-related argument for that function.
#'
#' Entries with `"DEPRECATED"` are used to flag functions that should generally
#' be avoided in package code because they depend on the local system clock or
#' timezone.
#'
#' @format
#' A named list of function/argument pairs.
#'
#' @examples
#' str(timezoneLintRules)
#'
#' @docType data
#' @name timezoneLintRules
#' @export
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
  "dmy" = "tz",
  "dmy_h" = "tz",
  "dmy_hm" = "tz",
  "dmy_hms" = "tz",
  "dym" = "tz",
  "fast_strptime" = "tz",
  "force_tz" = "tzone",
  "force_tzs" = "tzone_out",
  "interval" = "tzone",
  "local_time" = "tz",
  "make_datetime" = "tz",
  "mdy" = "tz",
  "mdy_h" = "tz",
  "mdy_hm" = "tz",
  "mdy_hms" = "tz",
  "my" = "tz",
  "myd" = "tz",
  "now" = "tzone",
  "parse_date_time" = "tz",
  "parse_date_time2" = "tz",
  "today" = "tzone",
  "with_tz" = "tzone",
  "ydm" = "tz",
  "ydm_h" = "tz",
  "ydm_hm" = "tz",
  "ydm_hms" = "tz",
  "ym" = "tz",
  "ymd" = "tz",
  "ymd_h" = "tz",
  "ymd_hm" = "tz",
  "ymd_hms" = "tz",
  "yq" = "tz",
  # MazamaCoreUtils functions
  "dateRange" = "timezone",
  "timeRange" = "timezone",
  "timeStamp" = "timezone",
  "parseDatetime" = "timezone"
)
