test_that("Appropriate coercion works", {

  expect_identical(setIfNull("4", 0), 4)
  expect_type(setIfNull("4", 0), "double")

  expect_identical(setIfNull("4", 0L), 4L)
  expect_type(setIfNull("4", 0L), "integer")
  expect_identical(setIfNull(6, 0L), 6L)
  expect_type(setIfNull(6, 0L), "integer")

  expect_identical(setIfNull("TRUE", TRUE), TRUE)
  expect_identical(setIfNull("TRUE", FALSE), TRUE)
  expect_identical(setIfNull("true", FALSE), TRUE)
  expect_identical(setIfNull("FALSE", FALSE), FALSE)
  expect_identical(setIfNull("FALSE", TRUE), FALSE)
  expect_identical(setIfNull("false", TRUE), FALSE)
  expect_type(setIfNull("TRUE", FALSE), "logical")

  expect_identical(setIfNull(5.6, 0L), 5L)
  expect_type(setIfNull(5.6, 0L), "integer")

  expect_identical(setIfNull("3+5i", 0i), 3 + 5i)
  expect_identical(setIfNull("6", 0i), 6 + 0i)

})


test_that("Inappropriate coercion fails", {

  expect_error(setIfNull("hello", 5))

})


test_that("Default is properly set", {

  expect_identical(setIfNull(NULL, 5), 5)
  expect_identical(setIfNull(NULL, 5L), 5L)
  expect_identical(setIfNull(NULL, "5"), "5")
  expect_identical(setIfNull(NULL, "hello"), "hello")
  expect_identical(setIfNull(NULL, 3 + 5i), 3 + 5i)
  expect_identical(setIfNull(NULL, TRUE), TRUE)
  expect_identical(setIfNull(NULL, FALSE), FALSE)
  expect_identical(setIfNull(NULL, list(1)), list(1))

})
