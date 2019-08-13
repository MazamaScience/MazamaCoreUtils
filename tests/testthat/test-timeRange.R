test_that("timeRange() requires explicit timezone", {

  expect_error(timeRange(timezone = NULL))

})


test_that("Output is a two element POSIXct vector", {


  startTime <- ISOdatetime(2019, 08, 01, 2, 15, 45, tz = "America/Los_Angeles")
  endTime <- ISOdatetime(2019, 08, 7, 20, 30, 40, tz = "America/Los_Angeles")

  tlim <- timeRange(starttime = startTime, endtime = endTime, timezone = "America/Los_Angeles")

  expect_length(tlim, 2)
  expect_s3_class(tlim, "POSIXct")

})


test_that("Output has the correct timezone", {

  startTime <- ISOdatetime(2019, 08, 01, 2, 15, 45, tz = "America/Los_Angeles")
  endTime <- ISOdatetime(2019, 08, 7, 20, 30, 40, tz = "America/Los_Angeles")

  tlim_LA <- timeRange(starttime = startTime, endtime = endTime, timezone = "America/Los_Angeles")
  tlim_UTC <- timeRange(starttime = startTime, endtime = endTime, timezone = "UTC")

  expect_identical(attributes(tlim_LA)$tzone, "America/Los_Angeles")
  expect_identical(attributes(tlim_UTC)$tzone, "UTC")

})


test_that("First element in output is less than the second", {

  startTime <- ISOdatetime(2019, 08, 01, 2, 15, 45, tz = "America/Los_Angeles")
  endTime <- ISOdatetime(2019, 08, 7, 20, 30, 40, tz = "America/Los_Angeles")

  tlim <- timeRange(starttime = startTime, endtime = endTime, timezone = "America/Los_Angeles")
  tlim_rev <- timeRange(starttime = endTime, endtime = startTime, timezone = "America/Los_Angeles")

  expect_lt(tlim[1], tlim[2])
  expect_lt(tlim_rev[1], tlim_rev[2])

})


test_that("Non-POSIXct inputs work as expected", {

  startTime <- ISOdatetime(2019, 08, 01, 2, 15, 45, tz = "America/Los_Angeles")
  endTime <- ISOdatetime(2019, 08, 7, 20, 30, 40, tz = "America/Los_Angeles")

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
