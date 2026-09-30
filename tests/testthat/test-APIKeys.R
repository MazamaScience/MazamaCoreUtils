test_that("set and get work", {

  # Default to NULL
  expect_identical(getAPIKey("provider1"), NULL)

  # basic set
  setAPIKey("provider1", "key1")
  setAPIKey("provider2", "key2")

  # basic get
  expect_identical(getAPIKey("provider1"), "key1")
  expect_identical(getAPIKey("provider2"), "key2")

  # show keys
  expect_output(showAPIKeys(mask = FALSE), "key1")
  expect_output(showAPIKeys(mask = FALSE), "key2")

  # keys are masked by default
  setAPIKey("provider3", "abcd1234efgh")
  expect_output(showAPIKeys(), "abcd\\*\\*\\*\\*")
  expect_false(any(grepl("efgh", capture.output(showAPIKeys()))))
  expect_false(any(grepl("key1", capture.output(showAPIKeys()))))

  # old key is returned
  expect_identical(setAPIKey("provider1", "update1"), "key1")
  expect_identical(getAPIKey("provider1"), "update1")

})



