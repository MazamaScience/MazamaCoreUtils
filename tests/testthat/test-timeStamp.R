test_that("explicit arguments are required", {

  expect_error(dateSequence(datetime = NULL))
  expect_error(dateSequence(timezone = NULL))
  expect_error(dateSequence(unit = NULL))
  expect_error(dateSequence(style = NULL))

})

test_that("'ymdhms' output is correct", {

  datetime <- parseDatetime("2019-01-08 12:30:15", timezone = "UTC")

  expect_identical(timeStamp(datetime, "UTC", unit = "year"), "2019")
  expect_identical(timeStamp(datetime, "UTC", unit = "month"), "201901")
  expect_identical(timeStamp(datetime, "UTC", unit = "day"), "20190108")
  expect_identical(timeStamp(datetime, "UTC", unit = "hour"), "2019010812")
  expect_identical(timeStamp(datetime, "UTC", unit = "min"), "201901081230")
  expect_identical(timeStamp(datetime, "UTC", unit = "sec"), "20190108123015")
  expect_identical(timeStamp(datetime, "UTC", unit = "msec"), "20190108123015.000")

})

test_that("'ymdThms' output is correct", {

  datetime <- parseDatetime("2019-01-08 12:30:15", timezone = "UTC")

  expect_identical(timeStamp(datetime, "UTC", style = "ymdThms", unit = "year"), "2019")
  expect_identical(timeStamp(datetime, "UTC", style = "ymdThms", unit = "month"), "201901")
  expect_identical(timeStamp(datetime, "UTC", style = "ymdThms", unit = "day"), "20190108")
  expect_identical(timeStamp(datetime, "UTC", style = "ymdThms", unit = "hour"), "20190108T12")
  expect_identical(timeStamp(datetime, "UTC", style = "ymdThms", unit = "min"), "20190108T1230")
  expect_identical(timeStamp(datetime, "UTC", style = "ymdThms", unit = "sec"), "20190108T123015")
  expect_identical(timeStamp(datetime, "UTC", style = "ymdThms", unit = "msec"), "20190108T123015.000")

})

test_that("'julian' output is correct", {

  datetime <- parseDatetime("2019-01-08 12:30:15", timezone = "UTC")

  expect_identical(timeStamp(datetime, "UTC", style = "julian", unit = "year"), "2019")
  expect_identical(timeStamp(datetime, "UTC", style = "julian", unit = "month"), "2019001")
  expect_identical(timeStamp(datetime, "UTC", style = "julian", unit = "day"), "2019008")
  expect_identical(timeStamp(datetime, "UTC", style = "julian", unit = "hour"), "201900812")
  expect_identical(timeStamp(datetime, "UTC", style = "julian", unit = "min"), "20190081230")
  expect_identical(timeStamp(datetime, "UTC", style = "julian", unit = "sec"), "2019008123015")
  expect_identical(timeStamp(datetime, "UTC", style = "julian", unit = "msec"), "2019008123015.000")

})

test_that("'clock' output is correct", {

  datetime <- parseDatetime("2019-01-08 12:30:15", timezone = "UTC")

  expect_identical(timeStamp(datetime, "UTC", style = "clock", unit = "year"), "2019")
  expect_identical(timeStamp(datetime, "UTC", style = "clock", unit = "month"), "2019-01")
  expect_identical(timeStamp(datetime, "UTC", style = "clock", unit = "day"), "2019-01-08")
  expect_identical(timeStamp(datetime, "UTC", style = "clock", unit = "hour"), "2019-01-08T12")
  expect_identical(timeStamp(datetime, "UTC", style = "clock", unit = "min"), "2019-01-08T12:30")
  expect_identical(timeStamp(datetime, "UTC", style = "clock", unit = "sec"), "2019-01-08T12:30:15")
  expect_identical(timeStamp(datetime, "UTC", style = "clock", unit = "msec"), "2019-01-08T12:30:15.000")

})

test_that("timezones are respected", {

  # PST
  datetime <- parseDatetime("2019-01-08 12:30:15", timezone = "UTC")
  expect_identical(
    timeStamp(datetime, "America/Los_Angeles", style = "clock", unit = "sec"),
    "2019-01-08T04:30:15"
  )

  # PDT
  datetime <- parseDatetime("2019-06-08 12:30:15", timezone = "UTC")
  expect_identical(
    timeStamp(datetime, "America/Los_Angeles", style = "clock", unit = "sec"),
    "2019-06-08T05:30:15"
  )

})

