test_that("manageCache() tests sortBy values", {

  oneByte <- 1e-6

  expect_error({
    removedCount <- manageCache(
      tempdir(),
      extensions = ".MazamaCoreUtils-test",
      maxCacheSize = oneByte,
      sortBy = "bad"
    )
  }, NULL) # expects error

  expect_error({
    removedCount <- manageCache(
      tempdir(),
      extensions = ".MazamaCoreUtils-test",
      maxCacheSize = oneByte,
      sortBy = "atime"
    )
  }, NA) # expects no error

})

test_that("manageCache() doesn't remove files when maxCacheSize is big", {

  # setup
  oneTByte <- 1e6
  count <- 4

  for ( i in 1:count ) {
    write.csv(iris, tempfile(fileext = ".MazamaCoreUtils-test"))
  }

  removedCount <- manageCache(
    tempdir(),
    extensions = ".MazamaCoreUtils-test",
    maxCacheSize = oneTByte
  )

  expect_equal(removedCount, 0)

  # cleanup
  file.remove(list.files(
    tempdir(),
    pattern = ".MazamaCoreUtils-test",
    full.names = TRUE
  ))

})

test_that("manageCache() removes files when maxCacheSize is small", {

  # setup
  count <- 4
  oneByte <- 1e-6

  for ( i in 1:count ) {
    write.csv(iris, tempfile(fileext = ".MazamaCoreUtils-test"))
  }

  removedCount <- manageCache(
    tempdir(),
    extensions = ".MazamaCoreUtils-test",
    maxCacheSize = oneByte
  )

  expect_equal(removedCount, count)
})

test_that("manageCache() validates parameters before removing files", {

  cacheDir <- tempfile("cache")
  dir.create(cacheDir)
  on.exit(unlink(cacheDir, recursive = TRUE))

  file <- file.path(cacheDir, "old.csv")
  write.csv(data.frame(a = 1), file)
  Sys.setFileTime(file, Sys.time() - 10 * 86400)

  expect_error(
    manageCache(cacheDir, extensions = "csv", maxFileAge = 1, sortBy = "bad"),
    "should be one of"
  )
  expect_true(file.exists(file))

  expect_error(manageCache(cacheDir, extensions = "csv", maxCacheSize = "big"))
  expect_error(manageCache(cacheDir, extensions = "csv", maxFileAge = "old"))
  expect_true(file.exists(file))

})
