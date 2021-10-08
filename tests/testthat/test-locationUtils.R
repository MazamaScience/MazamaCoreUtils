
test_that("validateLonLat() works", {

  expect_true(validateLonLat(-120.325278, 47.423333))

  expect_error(validateLonLat())
  expect_error(validateLonLat("abc"))
  expect_error(validateLonLat(-120.325278))
  expect_error(validateLonLat(-120.325278, "abc"))
  expect_error(validateLonLat(-120.325278, -91))
  expect_error(validateLonLat(-120.325278, 91))
  expect_error(validateLonLat(-181, 47.423333))
  expect_error(validateLonLat(181, 47.423333))

})

test_that("validateLonsLats() works", {

  lons <- 60:80
  lats <- 20:40

  expect_true(validateLonsLats(lons, lats))

  expect_error(validateLonsLats())
  expect_error(validateLonsLats("abc"))
  expect_error(validateLonsLats(lons))

  lons[1] <- NA
  expect_error(validateLonsLats(lons, lats))
  lons[1] <- -181
  expect_error(validateLonsLats(lons, lats))
  lons[1] <- 181
  expect_error(validateLonsLats(lons, lats))

  lons[1] <- 60
  lats[1] <- NA
  expect_error(validateLonsLats(lons, lats))
  lons[1] <- -91
  expect_error(validateLonsLats(lons, lats))
  lons[1] <- 91
  expect_error(validateLonsLats(lons, lats))

})

test_that("createLocationID() validates input", {

  expect_error(createLocationID())
  expect_error(createLocationID("abc"))
  expect_error(createLocationID(-120.325278))
  expect_error(createLocationID(-120.325278, "abc"))

})

test_that("createLocationID() rounds properly", {

  # Wenatchee

  # Test significant digits
  expect_equal(
    createLocationID(-120.325278, 47.423333),
    "500cb09a70d3d981"
  )

  expect_equal(
    createLocationID(-120.3252780, 47.4233330),
    "500cb09a70d3d981"
  )

  # Test rounding
  expect_equal(
    createLocationID(-120.32527804, 47.42333304),
    "500cb09a70d3d981"
  )

  expect_equal(
    createLocationID(-120.32527796, 47.42333296),
    "500cb09a70d3d981"
  )

  expect_false(
    createLocationID(-120.32527806, 47.42333306) == "500cb09a70d3d981"
  )

  expect_false(
    createLocationID(-120.32527794, 47.42333294) == "500cb09a70d3d981"
  )

})
