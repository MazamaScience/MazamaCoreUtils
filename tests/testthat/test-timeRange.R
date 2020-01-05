test_that("timeRange() requires explicit timezone", {

  expect_error(timeRange(timezone = NULL))

})


test_that("Output is a two element POSIXct vector", {


  startTime <- ISOdatetime(2019, 08, 01, 2, 15, 45, tz = "America/Los_Angeles")
  endTime <- ISOdatetime(2019, 08, 8, 20, 30, 40, tz = "America/Los_Angeles")

  tlim <- timeRange(starttime = startTime, endtime = endTime, timezone = "America/Los_Angeles")

  expect_length(tlim, 2)
  expect_s3_class(tlim, "POSIXct")

})


test_that("Output has the correct timezone", {

  startTime <- ISOdatetime(2019, 08, 01, 2, 15, 45, tz = "America/Los_Angeles")
  endTime <- ISOdatetime(2019, 08, 8, 20, 30, 40, tz = "America/Los_Angeles")

  tlim_LA <- timeRange(starttime = startTime, endtime = endTime, timezone = "America/Los_Angeles")
  tlim_UTC <- timeRange(starttime = startTime, endtime = endTime, timezone = "UTC")

  expect_identical(attributes(tlim_LA)$tzone, "America/Los_Angeles")
  expect_identical(attributes(tlim_UTC)$tzone, "UTC")

})


test_that("First element in output is less than the second", {

  startTime <- ISOdatetime(2019, 08, 01, 2, 15, 45, tz = "America/Los_Angeles")
  endTime <- ISOdatetime(2019, 08, 8, 20, 30, 40, tz = "America/Los_Angeles")

  tlim <- timeRange(starttime = startTime, endtime = endTime, timezone = "America/Los_Angeles")
  tlim_rev <- timeRange(starttime = endTime, endtime = startTime, timezone = "America/Los_Angeles")

  expect_lt(tlim[1], tlim[2])
  expect_lt(tlim_rev[1], tlim_rev[2])

})


test_that("Non-POSIXct inputs work as expected", {

  startTime <- ISOdatetime(2019, 08, 01, 2, 15, 45, tz = "America/Los_Angeles")
  endTime <- ISOdatetime(2019, 08, 8, 20, 30, 40, tz = "America/Los_Angeles")

  startTime_string <- strftime(startTime, format = "%Y-%m-%d %H:%M:%S", tz = "America/Los_Angeles")
  starTime_numeric <- strftime(startTime, format = "%Y%m%d%H%M%S", tz = "America/Los_Angeles") %>% as.numeric()

  endTime_string <- strftime(endTime, format = "%Y-%m-%d %H:%M:%S", tz = "America/Los_Angeles")
  endTime_numeric <- strftime(endTime, format = "%Y%m%d%H%M%S", tz = "America/Los_Angeles") %>% as.numeric()

  tlim_posixct <- timeRange(starttime = startTime, endtime = endTime, timezone = "America/Los_Angeles")
  tlim_string <- timeRange(starttime = startTime_string, endtime = endTime_string, timezone = "America/Los_Angeles")
  tlim_numeric <- timeRange(starttime = starTime_numeric, endtime = endTime_numeric, timezone = "America/Los_Angeles")

  expect_identical(tlim_posixct, tlim_string)
  expect_identical(tlim_posixct, tlim_numeric)

})


test_that("unit, ceilingStart and ceilingEnd arguments work", {

  startTime <- ISOdatetime(2019, 08, 01, 2, 15, 45, tz = "America/Los_Angeles")
  endTime <- ISOdatetime(2019, 08, 8, 20, 30, 40, tz = "America/Los_Angeles")

  startMin <- ISOdatetime(2019, 08, 01, 2, 15, 00, tz = "America/Los_Angeles")
  startHour <- ISOdatetime(2019, 08, 01, 2, 00, 00, tz = "America/Los_Angeles")
  startDay <- ISOdatetime(2019, 08, 01, 00, 00, 00, tz = "America/Los_Angeles")

  startMinCeil <- ISOdatetime(2019, 08, 01, 2, 16, 00, tz = "America/Los_Angeles")
  startHourCeil <- ISOdatetime(2019, 08, 01, 3, 00, 00, tz = "America/Los_Angeles")
  startDayCeil <- ISOdatetime(2019, 08, 02, 00, 00, 00, tz = "America/Los_Angeles")

  endMin <- ISOdatetime(2019, 08, 8, 20, 30, 00, tz = "America/Los_Angeles")
  endHour <- ISOdatetime(2019, 08, 8, 20, 00, 00, tz = "America/Los_Angeles")
  endDay <- ISOdatetime(2019, 08, 8, 00, 00, 00, tz = "America/Los_Angeles")

  endMinCeil <- ISOdatetime(2019, 08, 8, 20, 31, 00, tz = "America/Los_Angeles")
  endHourCeil <- ISOdatetime(2019, 08, 8, 21, 00, 00, tz = "America/Los_Angeles")
  endDayCeil <- ISOdatetime(2019, 08, 9, 00, 00, 00, tz = "America/Los_Angeles")

  tlim <- timeRange(startTime, endTime, timezone = "America/Los_Angeles")

  tlimMin <- timeRange(startTime, endTime, timezone = "America/Los_Angeles", unit = "min")
  tlimHour <- timeRange(startTime, endTime, timezone = "America/Los_Angeles", unit = "hour")
  tlimDay <- timeRange(startTime, endTime, timezone = "America/Los_Angeles", unit = "day")

  tlimMinCeil <- timeRange(startTime, endTime, timezone = "America/Los_Angeles", unit = "min", ceilingStart = TRUE, ceilingEnd = TRUE)
  tlimHourCeil <- timeRange(startTime, endTime, timezone = "America/Los_Angeles", unit = "hour", ceilingStart = TRUE, ceilingEnd = TRUE)
  tlimDayCeil <- timeRange(startTime, endTime, timezone = "America/Los_Angeles", unit = "day", ceilingStart = TRUE, ceilingEnd = TRUE)

  expect_identical(tlim, c(startTime, endTime))
  expect_identical(tlimMin, c(startMin, endMin))
  expect_identical(tlimHour, c(startHour, endHour))
  expect_identical(tlimDay, c(startDay, endDay))
  expect_identical(tlimMinCeil, c(startMinCeil, endMinCeil))
  expect_identical(tlimHourCeil, c(startHourCeil, endHourCeil))
  expect_identical(tlimDayCeil, c(startDayCeil, endDayCeil))

})


