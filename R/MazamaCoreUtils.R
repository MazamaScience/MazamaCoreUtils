#' @docType package
#' @name MazamaCoreUtils
#' @title Mazama Science utility functions for building operational systems.
#' @description The MazamaCoreUtils package was created by MazamaScience to
#' regularize our work building R-based web services.
#'
#' The main goal of this package is to create an internally standardized set of
#' functions that we can use in various systems that are being run
#' operationally. Areas of functionality supported by this package include:
#'
#' \itemize{
#' \item{ python style logging }
#' \item{ simple error messaging }
#' \item{ cache management }
#' \item{ date parsing }
#' \item{ source code linting }
#' }
NULL

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
  "Sys.Date" = "DEPRECATED",
  "Sys.time" = "DEPRECATED",
  # lubridate functions
  "now" = "tzone",
  "parse_date_time" = "tz",
  "with_tz" = "tzone",
  "ymd" = "tz",
  "ymd_h" = "tz",
  "ymd_hm" = "tz",
  "ymd_hms" = "tz"
)
