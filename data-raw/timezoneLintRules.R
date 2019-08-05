## DESCRIPTION
#  See ?MazamaCoreUtils::timezoneLintRules

timezoneLintRules <- list(
  "parse_date_time" = "tz",
  "with_tz" = "tzone",
  "now" = "tzone",
  "strftime" = "tz"
)

usethis::use_data(timezoneLintRules, overwrite = TRUE)
