
# Test function to explore how tz linting processes files

outer_function <- function(param2 = NULL) {

  if (is.null(param2)) {
    param2 <- lubridate::now(tzone = "UTC")
  } else if (lubridate::is.POSIXct(param2)) {
    param2 <- lubridate::with_tz("UTC")
  }


  dt1 <- lubridate::parse_date_time(
    "2019-08-01 14:30:00",
    # tz = "UTC",
    orders = "YmdHMS"
  )


  print(
    strftime(x = lubridate::floor_date(param2, "day"), tz = "UTC")
  )


}
