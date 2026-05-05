test_that("setIfNull() returns default only when target is NULL", {

  expect_identical(setIfNull(NULL, "foo"), "foo")
  expect_identical(setIfNull(NULL, 10), 10)
  expect_identical(setIfNull(NULL, TRUE), TRUE)

})

test_that("setIfNull() returns supplied target unchanged", {

  expect_identical(setIfNull("15", 0), "15")
  expect_identical(setIfNull(10, "foo"), 10)
  expect_identical(setIfNull(TRUE, 0), TRUE)
  expect_identical(setIfNull(mean, 0), mean)
  expect_identical(setIfNull(c(1, 2, 3), 0), c(1, 2, 3))

})

test_that("setIfNull() optionally enforces output type with as.* functions", {

  expect_identical(
    setIfNull("15", 0, enforcedType = "double"),
    15
  )

  expect_identical(
    setIfNull(NULL, "15", enforcedType = "integer"),
    15L
  )

  expect_identical(
    setIfNull(1, 0, enforcedType = "character"),
    "1"
  )

  expect_identical(
    setIfNull("2025-01-01", NULL, enforcedType = "Date"),
    as.Date("2025-01-01")
  )

})

test_that("setIfNull() errors when requested coercion function does not exist", {

  expect_error(
    setIfNull(1, 0, enforcedType = "notAType"),
    "No coercion function 'as.notAType\\(\\)' found"
  )

})
