test_that("initializeLogging() stops when logDir is invalid", {

  expect_error(initializeLogging(), "argument 'logDir' must not be NULL.")

})

test_that("initializeLogging() creates files", {

  logDir <- tempdir()
  initializeLogging(logDir)
  logger.trace("TRACE message")
  logger.debug("DEBUG message")
  logger.info("INFO message")
  logger.error("ERROR message")

  expect_true(
    all(file.exists(
      file.path(logDir, "TRACE.log"),
      file.path(logDir, "DEBUG.log"),
      file.path(logDir, "INFO.log"),
      file.path(logDir, "ERROR.log")
    ))
  )

  file.remove(
    file.path(logDir, "TRACE.log"),
    file.path(logDir, "DEBUG.log"),
    file.path(logDir, "INFO.log"),
    file.path(logDir, "ERROR.log")
  )
})
