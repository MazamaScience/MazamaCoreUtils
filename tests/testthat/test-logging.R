# tests/testthat/test-logging.R

test_that("logging API functions exist", {
  expect_true(exists("logger.setup"))
  expect_true(exists("logger.isInitialized"))
  expect_true(exists("logger.setLevel"))
  expect_true(exists("logger.trace"))
  expect_true(exists("logger.debug"))
  expect_true(exists("logger.info"))
  expect_true(exists("logger.warn"))
  expect_true(exists("logger.error"))

  expect_true(is.function(logger.setup))
  expect_true(is.function(logger.isInitialized))
  expect_true(is.function(logger.setLevel))
})

test_that("logger calls error before initialization", {
  options("MazamaCoreUtils.logger.initialized" = FALSE)

  expect_error(
    logger.info("test")
  )
})

test_that("logger.isInitialized reflects setup state", {
  options("MazamaCoreUtils.logger.initialized" = FALSE)
  expect_false(logger.isInitialized())

  logger.setup()
  expect_true(logger.isInitialized())
})

test_that("logger.setup is idempotent and overwrites configured log files", {
  tf <- tempfile(fileext = ".log")

  logger.setup(traceLog = tf)
  logger.setLevel(TRACE)
  logger.trace("first")

  expect_true(file.exists(tf))
  expect_true(length(readLines(tf, warn = FALSE)) >= 1)

  # Run setup again: should remove and recreate the file
  logger.setup(traceLog = tf)
  logger.setLevel(TRACE)
  logger.trace("second")

  lines <- readLines(tf, warn = FALSE)
  expect_true(length(lines) >= 1)
  expect_false(any(grepl("first", lines, fixed = TRUE)))
  expect_true(any(grepl("second", lines, fixed = TRUE)))
})

test_that("logger.setLevel affects console threshold but does not break file logging", {
  tf_debug <- tempfile(fileext = ".log")

  logger.setup(debugLog = tf_debug)
  logger.setLevel(ERROR)   # console should be quiet for DEBUG

  logger.debug("dmsg")

  expect_true(any(grepl("dmsg", readLines(tf_debug, warn = FALSE), fixed = TRUE)))
})

test_that("log lines are properly formatted", {

  tf <- tempfile(fileext = ".log")

  # Single TRACE-level file captures all messages
  logger.setup(
    traceLog = tf
  )
  logger.setLevel(TRACE)

  logger.error("test")
  logger.warn("test")
  logger.info("test")
  logger.debug("test")
  logger.trace("test")

  lines <- readLines(tf, warn = FALSE)

  # We expect at least these 5 messages
  expect_true(length(lines) >= 5)
  tail5 <- tail(lines, 5)

  # "<LEVEL> [YYYY-MM-DD HH:MM:SS UTC] test"
  pattern <- "\\[[0-9-]{10} [0-9:]{8} UTC\\] test$"

  expect_match(tail5[1], paste0("^ERROR ", pattern))
  expect_match(tail5[2], paste0("^WARN  ", pattern))  # padded
  expect_match(tail5[3], paste0("^INFO  ", pattern))  # padded
  expect_match(tail5[4], paste0("^DEBUG ", pattern))
  expect_match(tail5[5], paste0("^TRACE ", pattern))

})
