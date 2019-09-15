test_that("dateRange() requires explicit timezone", {

  expect_error(dateRange(timezone = NULL))

})

test_that("Output is a two element POSIXct vector", {


  startTime <- ISOdatetime(2019, 08, 01, 2, 15, 45, tz = "America/Los_Angeles")
  endTime <- ISOdatetime(2019, 08, 08, 20, 30, 40, tz = "America/Los_Angeles")

  tlim <- dateRange(startdate = startTime, enddate = endTime, timezone = "America/Los_Angeles")

  expect_length(tlim, 2)
  expect_s3_class(tlim, "POSIXct")

})


test_that("Output has the correct timezone", {

  startTime <- ISOdatetime(2019, 08, 01, 2, 15, 45, tz = "America/Los_Angeles")
  endTime <- ISOdatetime(2019, 08, 08, 20, 30, 40, tz = "America/Los_Angeles")

  tlim_LA <- dateRange(startdate = startTime, enddate = endTime, timezone = "America/Los_Angeles")
  tlim_UTC <- dateRange(startdate = startTime, enddate = endTime, timezone = "UTC")

  expect_identical(attributes(tlim_LA)$tzone, "America/Los_Angeles")
  expect_identical(attributes(tlim_UTC)$tzone, "UTC")

})


test_that("Equivalent inputs create the same output", {

  startTime <- ISOdatetime(2019, 08, 01, 2, 15, 45, tz = "America/Los_Angeles")
  endTime <- ISOdatetime(2019, 08, 08, 20, 30, 40, tz = "America/Los_Angeles")
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
  endTime <- ISOdatetime(2019, 08, 08, 20, 30, 40, tz = "America/Los_Angeles")

  tlim <- dateRange(startdate = startTime, enddate = endTime, timezone = "America/Los_Angeles")
  tlim_now <- dateRange(timezone = "America/Los_Angeles")
  tlim_rev <- dateRange(startdate = endTime, enddate = startTime, timezone = "America/Los_Angeles")

  expect_lt(tlim[1], tlim[2])
  expect_lt(tlim_now[1], tlim_now[2])
  expect_lt(tlim_rev[1], tlim_rev[2])

})


test_that("POSIXct inputs retain their point-in-time", {

  startTime <- ISOdatetime(2019, 08, 01, 2, 15, 45, tz = "America/Los_Angeles")
  endTime <- ISOdatetime(2019, 08, 08, 20, 30, 40, tz = "America/Los_Angeles")

  startTimeString <- strftime(startTime, format = "%Y%m%d%H%M%S", tz = "America/Los_Angeles")
  endTimeString <- strftime(endTime, format = "%Y%m%d%H%M%S", tz = "America/Los_Angeles")

  expectedStartTime_UTC <-
    startTime %>%
    lubridate::with_tz(tzone = "UTC") %>%
    lubridate::floor_date(unit = "day")

  expectedEndTime_UTC <-
    endTime %>%
    lubridate::with_tz(tzone = "UTC") %>%
    lubridate::floor_date(unit = "day") %>%
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
  endTime <- ISOdatetime(2019, 08, 08, 20, 30, 40, tz = "America/Los_Angeles")

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
  endTime <- ISOdatetime(2019, 08, 08, 20, 30, 40, tz = "America/Los_Angeles")

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


test_that("Default arguments work as expected", {

  startTime <- ISOdatetime(2019, 08, 1, 2, 15, 45, tz = "America/Los_Angeles")
  endTime <- ISOdatetime(2019, 08, 08, 20, 30, 40, tz = "America/Los_Angeles")

  numDays <- 7

  expectedStart <- lubridate::floor_date(startTime, unit = "day")
  expectedEnd <- lubridate::floor_date(endTime, unit = "day")

  expect_identical(
    dateRange(
      startdate = startTime, enddate = NULL,
      timezone = "America/Los_Angeles", unit = "day"
    ),
    c(expectedStart, expectedEnd)
  )

  expect_identical(
    dateRange(
      startdate = NULL, enddate = endTime,
      timezone = "America/Los_Angeles", unit = "day"
    ),
    c(expectedStart, expectedEnd)
  )

  expect_identical(
    dateRange(
      startdate = NULL, enddate = NULL,
      timezone = "America/Los_Angeles", unit = "day"
    ),
    c(
      lubridate::now(tzone = "America/Los_Angeles") %>%
        lubridate::floor_date(unit = "day") %>%
        `-`(lubridate::days(numDays)),
      lubridate::now(tzone = "America/Los_Angeles") %>%
        lubridate::floor_date(unit = "day")
    )
  )

})

test_that("unit and ceilingEnd arguments work", {

  startTime <- ISOdatetime(2019, 08, 01, 2, 15, 45, tz = "America/Los_Angeles")
  endTime <- ISOdatetime(2019, 08, 08, 20, 30, 40, tz = "America/Los_Angeles")

  startDate <- ISOdatetime(2019, 08, 01, 00, 00, 00, tz = "America/Los_Angeles")
  endDate <- ISOdatetime(2019, 08, 7, 23, 59, 59, tz = "America/Los_Angeles")

  startMin <- ISOdatetime(2019, 08, 01, 00, 00, 00, tz = "America/Los_Angeles")
  startHour <- ISOdatetime(2019, 08, 01, 00, 00, 00, tz = "America/Los_Angeles")
  startDay <- ISOdatetime(2019, 08, 01, 00, 00, 00, tz = "America/Los_Angeles")

  endMin <- ISOdatetime(2019, 08, 7, 23, 59, 00, tz = "America/Los_Angeles")
  endHour <- ISOdatetime(2019, 08, 7, 23, 00, 00, tz = "America/Los_Angeles")
  endDay <- ISOdatetime(2019, 08, 8, 00, 00, 00, tz = "America/Los_Angeles")

  endMinCeil <- ISOdatetime(2019, 08, 08, 23, 59, 00, tz = "America/Los_Angeles")
  endHourCeil <- ISOdatetime(2019, 08, 08, 23, 00, 00, tz = "America/Los_Angeles")
  endDayCeil <- ISOdatetime(2019, 08, 09, 00, 00, 00, tz = "America/Los_Angeles")

  tlim <- dateRange(startTime, endTime, timezone = "America/Los_Angeles")

  tlimMin <- dateRange(startTime, endTime, timezone = "America/Los_Angeles", unit = "min")
  tlimHour <- dateRange(startTime, endTime, timezone = "America/Los_Angeles", unit = "hour")
  tlimDay <- dateRange(startTime, endTime, timezone = "America/Los_Angeles", unit = "day")

  tlimMinCeil <- dateRange(startTime, endTime, timezone = "America/Los_Angeles", unit = "min", ceilingEnd = TRUE)
  tlimHourCeil <- dateRange(startTime, endTime, timezone = "America/Los_Angeles", unit = "hour", ceilingEnd = TRUE)
  tlimDayCeil <- dateRange(startTime, endTime, timezone = "America/Los_Angeles", unit = "day", ceilingEnd = TRUE)

  expect_identical(tlim, c(startDate, endDate))
  expect_identical(tlimMin, c(startMin, endMin))
  expect_identical(tlimHour, c(startHour, endHour))
  expect_identical(tlimDay, c(startDay, endDay))
  expect_identical(tlimMinCeil, c(startMin, endMinCeil))
  expect_identical(tlimHourCeil, c(startHour, endHourCeil))
  expect_identical(tlimDayCeil, c(startDay, endDayCeil))

})

test_that("days are honored when start or endtime is missing", {

  startTime <- ISOdatetime(2019, 08, 01, 2, 15, 45, tz = "America/Los_Angeles")
  endTime <- ISOdatetime(2019, 08, 08, 20, 30, 40, tz = "America/Los_Angeles")

  startDate <- ISOdatetime(2019, 08, 01, 00, 00, 00, tz = "America/Los_Angeles")
  endDate <- ISOdatetime(2019, 08, 7, 23, 59, 59, tz = "America/Los_Angeles")
  endDateCeil <- ISOdatetime(2019, 08, 8, 23, 59, 59, tz = "America/Los_Angeles")

  # Normal usage
  tlim <- dateRange(startTime, endTime,
                    timezone = "America/Los_Angeles")
  expect_identical(7, round(as.numeric(difftime(tlim[2], tlim[1], units="days"))))

  # Full end enddate
  tlim <- dateRange(startTime, endTime, ceilingEnd = TRUE,
                    timezone = "America/Los_Angeles")
  expect_identical(8, round(as.numeric(difftime(tlim[2], tlim[1], units="days"))))

  # Ignore days when both start and end are specified
  tlim <- dateRange(startTime, endTime, days = 5,
                    timezone = "America/Los_Angeles")
  expect_identical(7, round(as.numeric(difftime(tlim[2], tlim[1], units="days"))))

  tlim <- dateRange(startTime, endTime, ceilingEnd = TRUE,
                    timezone = "America/Los_Angeles")
  expect_identical(8, round(as.numeric(difftime(tlim[2], tlim[1], units="days"))))

  # Honor days when startdate is missing
  tlim <- dateRange(enddate = endTime, days = 3,
                    timezone = "America/Los_Angeles")
  expect_identical(3, round(as.numeric(difftime(tlim[2], tlim[1], units="days"))))

  # Honor both days and ceilingEnd, adjusting starttime
  tlim <- dateRange(enddate = endTime, ceilingEnd = TRUE, days = 3,
                    timezone = "America/Los_Angeles")
  expect_identical(3, round(as.numeric(difftime(tlim[2], tlim[1], units="days"))))
  expect_identical(endDateCeil, tlim[2])

  # Honor days when enddate is missing
  tlim <- dateRange(startdate = startTime, days = 3,
                    timezone = "America/Los_Angeles")
  expect_identical(3, round(as.numeric(difftime(tlim[2], tlim[1], units="days"))))

  # Honor days but ignore ceilingEnd when enddate is not specified
  tlim <- dateRange(startdate = startTime, ceilingEnd = TRUE, days = 3,
                    timezone = "America/Los_Angeles")
  expect_identical(3, round(as.numeric(difftime(tlim[2], tlim[1], units="days"))))



})

