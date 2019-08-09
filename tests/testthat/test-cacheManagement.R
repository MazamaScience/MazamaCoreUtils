test_that("manageCache() tests sortBy values", {

  oneByte <- 1e-6

  expect_error({
    removedCount <- manageCache(
      tempdir(),
      extensions = ".MazamaWebUtils-test",
      maxCacheSize = oneByte,
      sortBy = "a"
    )
  }, NULL) # expects error

  expect_error({
    removedCount <- manageCache(
      tempdir(),
      extensions = ".MazamaWebUtils-test",
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
    write.csv(iris, tempfile(fileext = ".MazamaWebUtils-test"))
  }

  removedCount <- manageCache(
    tempdir(),
    extensions = ".MazamaWebUtils-test",
    maxCacheSize = oneTByte
  )

  expect_equal(removedCount, 0)

  # cleanup
  file.remove(list.files(
    tempdir(),
    pattern = ".MazamaWebUtils-test",
    full.names = TRUE
  ))

})

test_that("manageCache() removes files when maxCacheSize is small", {

  # setup
  count <- 4
  oneByte <- 1e-6

  for ( i in 1:count ) {
    write.csv(iris, tempfile(fileext = ".MazamaWebUtils-test"))
  }

  removedCount <- manageCache(
    tempdir(),
    extensions = ".MazamaWebUtils-test",
    maxCacheSize = oneByte
  )

  expect_equal(removedCount, count)
})
