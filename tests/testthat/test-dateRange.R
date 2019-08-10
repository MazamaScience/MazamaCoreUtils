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
  tlim_now <- dateRange(startdate = startTime, enddate = endTime, timezone = "America/Los_Angeles")

  expect_lt(tlim[1], tlim[2])
  expect_lt(tlim_now[1], tlim_now[2])

})


test_that("POSIXct inputs retain their point-in-time", {

  startTime <- ISOdatetime(2019, 08, 01, 2, 15, 45, tz = "America/Los_Angeles")
  endTime <- ISOdatetime(2019, 08, 7, 20, 30, 40, tz = "America/Los_Angeles")

  # Test with POSIXct inputs for start/end


  # Test with POSIXct input for start


  # Test with POSIXct input for end


})


test_that("End-of-Day unit works as expected", {


})
