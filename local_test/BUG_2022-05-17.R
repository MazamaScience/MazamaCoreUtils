# BUG on 2022-05-17

datetime = lubridate::now(tzone = "UTC")
by = "day"
url = "https://wrcc.dri.edu/cgi-bin/smoke.pl"

year <- strftime(datetime, "%y", tz = "UTC") # NOTE:  year without century
month <- strftime(datetime, "%m", tz = "UTC")
day <- strftime(datetime, "%d", tz = "UTC")
ymd <- paste0(year, month, day)

# ----- Download current monitors page ---------------------------------------

# Create CGI parameters
.params <- list(
  mon = month,
  day = day,
  yea = year,
  Update = "Update"
)

suppressWarnings({
  r <- httr::POST(url, body = .params)
})

if ( httr::http_error(r) ) {
  cat(httr::content(r))
}

# NOTE:  Non-standard return needs to be handled carefully
# TODO:  Figure out which non-standard encoding is being used to encode "micro sign"
fileString <-
  httr::content(r, as = "raw", type = "text/html", encoding = "UTF-8") %>%
  rawToChar()

# ----- Get unit IDs ---------------------------------------------------------

# This seems to fail just after 6pm when the table on the page has no rows

a <- html_getLinks(fileString)

# NOTE:   Use chrome devtools to review the web page for link construction

# matchMatrix <-
#   MazamaCoreUtils::html_getLinkUrls(fileString) %>%
#   stringr::str_subset(pattern = "rawMAIN4\\.pl") %>%
#   stringr::str_match(pattern = "^.*\\?id(....).*$")
#
# print(matchMatrix[,2])
#

