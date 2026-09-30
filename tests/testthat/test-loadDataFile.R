# Helper: save objects to an .rda file in a fresh temporary directory
.makeDataDir <- function(filename = "test.rda", ...) {
  dataDir <- tempfile("dataDir")
  dir.create(dataDir)
  objects <- list(...)
  env <- list2env(objects)
  save(list = names(objects), file = file.path(dataDir, filename), envir = env)
  dataDir
}

test_that("loadDataFile() loads from dataDir", {

  dataDir <- .makeDataDir(myData = data.frame(a = 1:3))
  on.exit(unlink(dataDir, recursive = TRUE))

  result <- loadDataFile("test.rda", dataDir = dataDir)
  expect_equal(result, data.frame(a = 1:3))

})

test_that("loadDataFile() loads from dataUrl", {

  dataDir <- .makeDataDir(myData = "from url")
  on.exit(unlink(dataDir, recursive = TRUE))
  dataUrl <- paste0("file://", dataDir)

  expect_identical(loadDataFile("test.rda", dataUrl = dataUrl), "from url")

})

test_that("loadDataFile() requires exactly one object in the file", {

  dataDir <- .makeDataDir(first = "one", second = "two")
  on.exit(unlink(dataDir, recursive = TRUE))

  expect_error(loadDataFile("test.rda", dataDir = dataDir), "exactly one object")
  expect_error(
    loadDataFile("test.rda", dataUrl = paste0("file://", dataDir)),
    "exactly one object"
  )

})

test_that("loadDataFile() validates parameters", {

  expect_error(loadDataFile(), "filename")
  expect_error(loadDataFile("test.rda"), "dataUrl.*dataDir")
  expect_error(
    loadDataFile("test.rda", dataDir = tempdir(), priority = "bad")
  )

})

test_that("loadDataFile() fails clearly when the file cannot be loaded", {

  dataDir <- .makeDataDir(myData = 1)
  on.exit(unlink(dataDir, recursive = TRUE))

  expect_error(
    loadDataFile("missing.rda", dataDir = dataDir),
    "could not be loaded"
  )
  expect_error(
    loadDataFile("test.rda", dataDir = file.path(dataDir, "nonexistent")),
    "does not exist"
  )
  expect_error(
    loadDataFile("missing.rda", dataUrl = paste0("file://", dataDir)),
    "could not be loaded"
  )

})

test_that("loadDataFile() falls back to the other source based on priority", {

  goodDir <- .makeDataDir(myData = "good")
  badDir <- tempfile("badDir")
  dir.create(badDir)
  on.exit(unlink(c(goodDir, badDir), recursive = TRUE))

  goodUrl <- paste0("file://", goodDir)
  badUrl <- paste0("file://", badDir)

  # dataDir first, bad dataDir falls back to dataUrl
  expect_identical(
    loadDataFile("test.rda", dataDir = badDir, dataUrl = goodUrl, priority = "dataDir"),
    "good"
  )

  # dataUrl first, bad dataUrl falls back to dataDir
  expect_identical(
    loadDataFile("test.rda", dataDir = goodDir, dataUrl = badUrl, priority = "dataUrl"),
    "good"
  )

  # both sources bad
  expect_error(
    loadDataFile("test.rda", dataDir = badDir, dataUrl = badUrl),
    "dataDir or dataUrl"
  )

})
