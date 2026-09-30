test_that(".validateTimezone() accepts valid timezones", {

  expect_identical(.validateTimezone("UTC"), "UTC")
  expect_invisible(.validateTimezone("America/Los_Angeles"))

})

test_that(".validateTimezone() rejects invalid timezones", {

  expect_error(.validateTimezone("Not/AZone"), "not found in OlsonNames")
  expect_error(.validateTimezone(""), "not found in OlsonNames")
  expect_error(.validateTimezone(NA), "character string of length one")
  expect_error(.validateTimezone(1), "character string of length one")
  expect_error(.validateTimezone(c("UTC", "UTC")), "character string of length one")
  expect_error(.validateTimezone(character(0)), "character string of length one")

})

test_that("date-time functions share timezone validation", {

  bad <- c("UTC", "America/Los_Angeles")

  expect_error(parseDatetime("20190108", timezone = bad), "length one")
  expect_error(timeRange("20190108", "20190109", timezone = bad), "length one")
  expect_error(dateRange("20190108", timezone = bad), "length one")
  expect_error(dateSequence("20190108", "20190109", timezone = bad), "length one")
  expect_error(timeStamp(timezone = bad), "length one")

  expect_error(parseDatetime("20190108", timezone = "Not/AZone"), "OlsonNames")
  expect_error(timeRange("20190108", "20190109", timezone = "Not/AZone"), "OlsonNames")
  expect_error(dateRange("20190108", timezone = "Not/AZone"), "OlsonNames")
  expect_error(dateSequence("20190108", "20190109", timezone = "Not/AZone"), "OlsonNames")
  expect_error(timeStamp(timezone = "Not/AZone"), "OlsonNames")

})
