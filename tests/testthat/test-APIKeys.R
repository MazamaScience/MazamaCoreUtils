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
  expect_identical(showAPIKeys(), str(list(provider1 = "key1", provider2 = "key2")))

  # old key is returned
  expect_identical(setAPIKey("provider1", "update1"), "key1")
  expect_identical(getAPIKey("provider1"), "update1")

})



