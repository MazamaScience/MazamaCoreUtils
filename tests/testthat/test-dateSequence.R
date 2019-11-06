test_that("explicit arguments are required", {

  expect_error(dateSequence(startdate = NULL))
  expect_error(dateSequence(enddate = NULL))
  expect_error(dateSequence(timezone = NULL))
  expect_error(dateSequence(ceilingEnd = NULL))

})

test_that("daylight savings is handled and ceilingEnd works", {

  startdate <- 20191101
  enddate <- 20191107

  localDatetimes_8 <- c(
    ISOdatetime(2019, 11, 01, 00, 00, 00, tz = "America/Los_Angeles"),
    ISOdatetime(2019, 11, 02, 00, 00, 00, tz = "America/Los_Angeles"),
    ISOdatetime(2019, 11, 03, 00, 00, 00, tz = "America/Los_Angeles"),
    ISOdatetime(2019, 11, 04, 00, 00, 00, tz = "America/Los_Angeles"),
    ISOdatetime(2019, 11, 05, 00, 00, 00, tz = "America/Los_Angeles"),
    ISOdatetime(2019, 11, 06, 00, 00, 00, tz = "America/Los_Angeles"),
    ISOdatetime(2019, 11, 07, 00, 00, 00, tz = "America/Los_Angeles"),
    ISOdatetime(2019, 11, 08, 00, 00, 00, tz = "America/Los_Angeles")
  )

  localDatetimes_7 <- localDatetimes_8[1:7]

  datetime_LA_7 <- dateSequence(startdate = startdate, enddate = enddate,
                              timezone = "America/Los_Angeles")
  datetime_LA_8 <- dateSequence(startdate = startdate, enddate = enddate,
                              timezone = "America/Los_Angeles",
                              ceilingEnd = TRUE)

  expect_identical(datetime_LA_7, localDatetimes_7)
  expect_identical(datetime_LA_8, localDatetimes_8)

})

test_that("POSIXct preserves instant in time", {

  startdate <- 20190308
  enddate <- 20190312

  datetime_JST_5 <- c(
    ISOdatetime(2019, 03, 08, 00, 00, 00, tz = "Asia/Tokyo"),
    ISOdatetime(2019, 03, 09, 00, 00, 00, tz = "Asia/Tokyo"),
    ISOdatetime(2019, 03, 10, 00, 00, 00, tz = "Asia/Tokyo"),
    ISOdatetime(2019, 03, 11, 00, 00, 00, tz = "Asia/Tokyo"),
    ISOdatetime(2019, 03, 12, 00, 00, 00, tz = "Asia/Tokyo")
  )

  datetime_UTC_5 <- c(
    ISOdatetime(2019, 03, 07, 00, 00, 00, tz = "UTC"),
    ISOdatetime(2019, 03, 08, 00, 00, 00, tz = "UTC"),
    ISOdatetime(2019, 03, 09, 00, 00, 00, tz = "UTC"),
    ISOdatetime(2019, 03, 10, 00, 00, 00, tz = "UTC"),
    ISOdatetime(2019, 03, 11, 00, 00, 00, tz = "UTC")
  )

  # Passing in POSIXct values preserves the instant in time before flooring --
  #   midnight Tokyo time is the day before in UTC
  JST <- dateSequence(startdate, enddate, timezone = "Asia/Tokyo")
  UTC <- dateSequence(JST[1], JST[5], timezone = "UTC")

  expect_identical(JST, datetime_JST_5)
  expect_identical(UTC, datetime_UTC_5)

})

