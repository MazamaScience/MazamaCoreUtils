test_that("Stop on all NA output", {

  input <- c(NA, "10-16-2018", "10/16/18")

  expect_error(
    parseDatetime(input),
    "argument 'timezone' must not be NULL."
  )

  expect_error(
    parseDatetime(input, timezone = "America/Los_Angeles"),
    "No datetimes could be parsed."
  )

  expect_error(
    parseDatetime(input, timezone = "America/Los_Angeles", expectAll = TRUE),
    "No datetimes could be parsed."
  )

})

test_that("POSIXct inputs are unaltered", {

  input <- seq(ISOdate(2018, 10, 12, tz = "UTC"), by = "day", length.out = 7)

  expect_equal(parseDatetime(input, timezone = "UTC"), input)
  expect_equal(
    parseDatetime(input, timezone = "America/Los_Angeles"),
    lubridate::with_tz(input, tzone = "America/Los_Angeles")
  )

})

test_that("Fail on some NA outputs when ExpectAll is true", {

  input <- c("20181013", NA, "20181015", "181016", "10172018")

  expect_error(
    parseDatetime(input, timezone = "America/Los_Angeles", expectAll = TRUE),
    "2 datetimes failed to parse (at indices: 4, 5).", fixed = TRUE
  )

})

test_that("Fractional Julian days are supported", {

  # To support of strings found in NASA GOES filenames
  input <- c(2019249182609, 20192491826095, 201924918260951, 2019249182609513)
  output <- c("2019-09-06 18:26:09.000", "2019-09-06 18:26:09.500",
              "2019-09-06 18:26:09.509", "2019-09-06 18:26:09.513")

  datetime <- parseDatetime(input, timezone = "UTC", isJulian = TRUE)

  expect_equal(
    strftime(datetime, "%Y-%m-%d %H:%M:%OS3", tz = "UTC"),
    output
  )

})

test_that("All orders from Y to YmdHMS are supported", {

  input <- c(2018, 201810, 20181016, 2018101612, 201810161215, 20181016121530)
  output <- c("2018-01-01 00:00:00", "2018-10-01 00:00:00", "2018-10-16 00:00:00",
              "2018-10-16 12:00:00", "2018-10-16 12:15:00", "2018-10-16 12:15:30")

  datetime <- parseDatetime(input, timezone = "UTC")

  expect_equal(
    strftime(datetime, "%Y-%m-%d %H:%M:%S", tz = "UTC"),
    output
  )

})

test_that("All orders from Y to YjHMS are supported for isJulian = TRUE", {

  input <- c(2018, 2018289, 201828912, 20182891215, 2018289121530)
  output <- c("2018-01-01 00:00:00", "2018-10-16 00:00:00",
              "2018-10-16 12:00:00", "2018-10-16 12:15:00", "2018-10-16 12:15:30")

  datetime <- parseDatetime(input, isJulian = TRUE, timezone = "UTC")

  expect_equal(
    strftime(datetime, "%Y-%m-%d %H:%M:%S", tz = "UTC"),
    output
  )

})
