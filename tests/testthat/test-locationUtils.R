
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

test_that("createLocationMask() validates input", {

  # Generate errors
  expect_error(createLocationMask())
  expect_error(createLocationMask("abc"))
  expect_error(createLocationMask(-120.325278))
  expect_error(createLocationMask(1:4, 1:5))
  expect_error(createLocationMask(1, 1, lonRange = c(-200, -120)))
  expect_error(createLocationMask(1, 1, latRange = c(-200, 500)))

  # Acceptable bad input
  expect_equal(createLocationMask(-120.325278, "abc"), FALSE)
  expect_equal(createLocationMask(-120.325278, NA), FALSE)

})

test_that("createLocationMask() works", {

  # removeZeroZero = TRUE
  expect_equal(
    createLocationMask(
      longitude = c(1,NA,1,0,1,200,  1,1,1,0,1,1),
      latitude = c(1,1,1,1,1,1,  1,NA,1,0,1,200)
    ),
    c(TRUE, FALSE, TRUE, TRUE, TRUE, FALSE,
      TRUE, FALSE, TRUE, FALSE, TRUE, FALSE)
  )

  # removeZeroZero = FALSE
  expect_equal(
    createLocationMask(
      longitude = c(1,NA,1,0,1,200,  1,1,1,0,1,1),
      latitude = c(1,1,1,1,1,1,  1,NA,1,0,1,200),
      removeZeroZero = FALSE
    ),
    c(TRUE, FALSE, TRUE, TRUE, TRUE, FALSE,
      TRUE, FALSE, TRUE, TRUE, TRUE, FALSE)
  )

})

