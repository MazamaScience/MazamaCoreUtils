test_that("Non-null inputs are returned invisibly", {

  expect_invisible(stopIfNull(10))
  expect_invisible(stopIfNull("bar"))
  expect_invisible(stopIfNull(mtcars))

  expect_identical(stopIfNull(5), 5)
  expect_identical(stopIfNull("foo"), "foo")
  expect_identical(stopIfNull(mtcars), mtcars)

})


test_that("Error messages work as expected", {

  customMessage <- "This is a message!"

  expect_error(stopIfNull(NULL))
  expect_error(stopIfNull(NULL, msg = customMessage), customMessage)

})
