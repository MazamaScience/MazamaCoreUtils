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

test_that("manageCache() reports files it cannot remove", {

  skip_on_os("windows")

  cacheDir <- tempfile("cache")
  dir.create(cacheDir)
  on.exit({
    Sys.chmod(cacheDir, "755")
    unlink(cacheDir, recursive = TRUE)
  })

  for ( name in c("a.csv", "b.csv") ) {
    file <- file.path(cacheDir, name)
    write.csv(data.frame(a = 1), file)
    Sys.setFileTime(file, Sys.time() - 10 * 86400)
  }

  # A read-only directory prevents removal of the files inside it
  Sys.chmod(cacheDir, "555")
  skip_if(file.access(cacheDir, 2) == 0, "cannot make directory read-only")

  expect_warning(
    removed <- manageCache(cacheDir, extensions = "csv", maxFileAge = 1),
    "could not be removed"
  )

  expect_equal(removed, 0)
  expect_true(all(file.exists(file.path(cacheDir, c("a.csv", "b.csv")))))

})

test_that("manageCache() counts only files it actually removed", {

  cacheDir <- tempfile("cache")
  dir.create(cacheDir)
  on.exit(unlink(cacheDir, recursive = TRUE))

  for ( name in c("a.csv", "b.csv") ) {
    file <- file.path(cacheDir, name)
    write.csv(data.frame(a = 1), file)
    Sys.setFileTime(file, Sys.time() - 10 * 86400)
  }

  expect_equal(manageCache(cacheDir, extensions = "csv", maxFileAge = 1), 2)
  expect_length(list.files(cacheDir), 0)

})
