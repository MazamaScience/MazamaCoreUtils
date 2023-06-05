test_that("algorithms work", {

  # Setup
  longitude <- -120:-110
  latitude <- 30:40

  # Default to "digest"
  expect_identical(
    createLocationID(longitude, latitude),
    c("2579cca9bc8bb160","0bc60b264bab6c8f","242c3d44df97de47","891fb5f2df4f8a39",
      "1e9bb5a927f39726","890cb1a66d1e9e9d","e2105228a0188686","f61bfb636bba4233",
      "c60fc5cd3450730d","ef89fa02bbd43fb5","c389bbe887dcf75f")
  )

  # Explicit "digest"
  expect_identical(
    createLocationID(longitude, latitude),
    c("2579cca9bc8bb160","0bc60b264bab6c8f","242c3d44df97de47","891fb5f2df4f8a39",
      "1e9bb5a927f39726","890cb1a66d1e9e9d","e2105228a0188686","f61bfb636bba4233",
      "c60fc5cd3450730d","ef89fa02bbd43fb5","c389bbe887dcf75f")
  )

  # Explicit "geohash"
  expect_identical(
    createLocationID(longitude, latitude, "geohash"),
    c("9m6dtm6dtm","9me2k56u54","9msn4c7j88","9mug9x7nym","9qj92me2k5","9qnpp5e9cb",
      "9qquvceepq","9qxdkxeut5","9wb25msn4c","9wcjf5sr2w","9x1g8cu2yh")
  )

  # Stop on unexpected algorithm
  expect_error(createLocationID(longitude, latitude, "paste"))

})



