test_that("dateRange() requires explicit timezone", {

  expect_error(dateRange(timezone = NULL))

})

test_that("Output is a two element POSIXct vector", {


  startTime <- ISOdatetime(2019, 08, 01, 2, 15, 45, tz = "America/Los_Angeles")
  endTime <- ISOdatetime(2019, 08, 7, 20, 30, 40, tz = "America/Los_Angeles")

  tlim <- dateRange(startdate = startTime, enddate = endTime, timezone = "America/Los_Angeles")

  expect_length(tlim, 2)
  expect_s3_class(tlim, "POSIXct")

})


test_that("Output has the correct timezone", {

  startTime <- ISOdatetime(2019, 08, 01, 2, 15, 45, tz = "America/Los_Angeles")
  endTime <- ISOdatetime(2019, 08, 7, 20, 30, 40, tz = "America/Los_Angeles")

  tlim_LA <- dateRange(startdate = startTime, enddate = endTime, timezone = "America/Los_Angeles")
  tlim_UTC <- dateRange(startdate = startTime, enddate = endTime, timezone = "UTC")

  expect_identical(attributes(tlim_LA)$tzone, "America/Los_Angeles")
  expect_identical(attributes(tlim_UTC)$tzone, "UTC")

})


test_that("Equivalent inputs create the same output", {

  startTime <- ISOdatetime(2019, 08, 01, 2, 15, 45, tz = "America/Los_Angeles")
  endTime <- ISOdatetime(2019, 08, 7, 20, 30, 40, tz = "America/Los_Angeles")
  dayCount <- 7

  tlim_startEnd <- dateRange(startdate = startTime, enddate = endTime, timezone = "America/Los_Angeles")
  tlim_startDays <- dateRange(startdate = startTime, days = dayCount, timezone = "America/Los_Angeles")
  tlim_endDays <- dateRange(enddate = endTime, days = dayCount, timezone = "America/Los_Angeles")

  expect_identical(tlim_startEnd, tlim_startDays)
  expect_identical(tlim_startEnd, tlim_endDays)
  expect_equal(tlim_startDays, tlim_endDays)

})


test_that("First element in output is less than the second", {

  startTime <- ISOdatetime(2019, 08, 01, 2, 15, 45, tz = "America/Los_Angeles")
  endTime <- ISOdatetime(2019, 08, 7, 20, 30, 40, tz = "America/Los_Angeles")

  tlim <- dateRange(startdate = startTime, enddate = endTime, timezone = "America/Los_Angeles")
  tlim_now <- dateRange(timezone = "America/Los_Angeles")
  tlim_rev <- dateRange(startdate = endTime, enddate = startTime, timezone = "America/Los_Angeles")

  expect_lt(tlim[1], tlim[2])
  expect_lt(tlim_now[1], tlim_now[2])
  expect_lt(tlim_rev[1], tlim_rev[2])

})


test_that("POSIXct inputs retain their point-in-time", {

  startTime <- ISOdatetime(2019, 08, 01, 2, 15, 45, tz = "America/Los_Angeles")
  endTime <- ISOdatetime(2019, 08, 7, 20, 30, 40, tz = "America/Los_Angeles")

  startTimeString <- strftime(startTime, format = "%Y%m%d%H%M%S", tz = "America/Los_Angeles")
  endTimeString <- strftime(endTime, format = "%Y%m%d%H%M%S", tz = "America/Los_Angeles")

  expectedStartTime_UTC <- startTime %>%
    lubridate::with_tz(tzone = "UTC") %>%
    lubridate::floor_date(unit = "day")

  expectedEndTime_UTC <- endTime %>%
    lubridate::with_tz(tzone = "UTC") %>%
    lubridate::ceiling_date(unit = "day") %>%
    `-`(lubridate::dseconds(1))


  # Test with POSIXct inputs for start/end
  tlim <- dateRange(startdate = startTime, enddate = endTime, timezone = "UTC", unit = "sec")

  expect_identical(tlim[1], expectedStartTime_UTC)
  expect_identical(tlim[2], expectedEndTime_UTC)


  # Test with POSIXct input for start
  tlim <- dateRange(startdate = startTime, enddate = endTimeString, timezone = "UTC", unit = "sec")

  expect_identical(tlim[1], expectedStartTime_UTC)


  # Test with POSIXct input for end
  tlim <- dateRange(startdate = startTimeString, enddate = endTime, timezone = "UTC", unit = "sec")

  expect_identical(tlim[2], expectedEndTime_UTC)

})


test_that("Non-POSIXct inputs work as expected", {

  startTime <- ISOdatetime(2019, 08, 01, 2, 15, 45, tz = "America/Los_Angeles")
  endTime <- ISOdatetime(2019, 08, 7, 20, 30, 40, tz = "America/Los_Angeles")

  startTime_string <- strftime(startTime, format = "%Y-%m-%d %H:%M:%S", tz = "America/Los_Angeles")
  starTime_numeric <- strftime(startTime, format = "%Y%m%d%H%M%S", tz = "America/Los_Angeles") %>% as.numeric()

  endTime_string <- strftime(endTime, format = "%Y-%m-%d %H:%M:%S", tz = "America/Los_Angeles")
  endTime_numeric <- strftime(endTime, format = "%Y%m%d%H%M%S", tz = "America/Los_Angeles") %>% as.numeric()

  tlim_posixct <- dateRange(startdate = startTime, enddate = endTime, timezone = "America/Los_Angeles")
  tlim_string <- dateRange(startdate = startTime_string, enddate = endTime_string, timezone = "America/Los_Angeles")
  tlim_numeric <- dateRange(startdate = starTime_numeric, enddate = endTime_numeric, timezone = "America/Los_Angeles")

  expect_identical(tlim_posixct, tlim_string)
  expect_identical(tlim_posixct, tlim_numeric)

})


test_that("End-of-Day unit works as expected", {

  startTime <- ISOdatetime(2019, 08, 01, 2, 15, 45, tz = "America/Los_Angeles")
  endTime <- ISOdatetime(2019, 08, 7, 20, 30, 40, tz = "America/Los_Angeles")

  # Standard input
  tlim_sec <- dateRange(startdate = startTime, enddate = endTime, timezone = "UTC", unit = "sec")
  tlim_min <- dateRange(startdate = startTime, enddate = endTime, timezone = "UTC", unit = "min")
  tlim_hour <- dateRange(startdate = startTime, enddate = endTime, timezone = "UTC", unit = "hour")
  tlim_day <- dateRange(startdate = startTime, enddate = endTime, timezone = "UTC", unit = "day")

  expect_identical(strftime(tlim_sec[2], format = "%H%M%S", tz = "UTC"), "235959")
  expect_identical(strftime(tlim_min[2], format = "%H%M%S", tz = "UTC"), "235900")
  expect_identical(strftime(tlim_hour[2], format = "%H%M%S", tz = "UTC"), "230000")
  expect_identical(strftime(tlim_day[2], format = "%H%M%S", tz = "UTC"), "000000")

  # Reversed input
  rev_tlim <- dateRange(startdate = endTime, enddate = startTime, timezone = "UTC", unit = "sec")

  expect_identical(strftime(rev_tlim[2], format = "%H%M%S", tz = "UTC"), "235959")

})