test_that("createLocationID() validates input", {

  expect_error(createLocationID("abc"))
  expect_error(createLocationID(-120.325278))
  expect_error(createLocationID(1:4, 1:5))

})

test_that("createLocationID() handles invalid locations", {

  longitude <- c(-122.5, 0, NA, -122.5, -122.5)
  latitude <- c( 47.5, 0, 47.5,   NA, 47.5)
  algorithm <- "geohash"
  invalidID = "bad"

  expect_equal(
    createLocationID(longitude, latitude),
    c("c22yhrn5x1", NA, NA, NA, "c22yhrn5x1")
  )

  expect_equal(
    createLocationID(longitude, latitude, invalidID = "bad"),
    c("c22yhrn5x1", "bad", "bad", "bad", "c22yhrn5x1")
  )

})

test_that("createLocationID() rounds properly", {

  # Wenatchee

  # Test significant digits
  expect_equal(
    createLocationID(-120.325278, 47.423333),
    "c26mvcjucy"
  )

  expect_equal(
    createLocationID(-120.3252780, 47.4233330),
    "c26mvcjucy"
  )

  # Test rounding
  expect_equal(
    createLocationID(-120.32527804, 47.42333304),
    "c26mvcjucy"
  )

  expect_equal(
    createLocationID(-120.32527796, 47.42333296),
    "c26mvcjucy"
  )

  # Different enough
  expect_false(
    createLocationID(-120.3252, 47.4233) == "c26mvcjucy"
  )

})

test_that("createLocationID() algorithms work", {

  # Setup
  longitude <- -120:-110
  latitude <- 30:40

  # Default to "geohash"
  expect_identical(
    createLocationID(longitude, latitude),
    c("9m6dtm6dtm","9me2k56u54","9msn4c7j88","9mug9x7nym","9qj92me2k5","9qnpp5e9cb",
      "9qquvceepq","9qxdkxeut5","9wb25msn4c","9wcjf5sr2w","9x1g8cu2yh")
  )

  # Explicit "digest"
  expect_identical(
    createLocationID(longitude, latitude, algorithm = "digest"),
    c("2579cca9bc8bb160","0bc60b264bab6c8f","242c3d44df97de47","891fb5f2df4f8a39",
      "1e9bb5a927f39726","890cb1a66d1e9e9d","e2105228a0188686","f61bfb636bba4233",
      "c60fc5cd3450730d","ef89fa02bbd43fb5","c389bbe887dcf75f")
  )

  # Explicit "geohash"
  expect_identical(
    createLocationID(longitude, latitude, algorithm = "geohash"),
    c("9m6dtm6dtm","9me2k56u54","9msn4c7j88","9mug9x7nym","9qj92me2k5","9qnpp5e9cb",
      "9qquvceepq","9qxdkxeut5","9wb25msn4c","9wcjf5sr2w","9x1g8cu2yh")
  )

  # Stop on unexpected algorithm
  expect_error(createLocationID(longitude, latitude, algorithm = "paste"))

})



