test_that("log lines are properly formatted", {

  logger.setup()
  logger.setLevel(TRACE)

  expect_match(logger.fatal("test"), "FATAL \\[[0-9-]{10} [0-9:]{8}\\] test")
  expect_match(logger.error("test"), "ERROR \\[[0-9-]{10} [0-9:]{8}\\] test")
  expect_match(logger.warn("test"), "WARN \\[[0-9-]{10} [0-9:]{8}\\] test")
  expect_match(logger.info("test"), "INFO \\[[0-9-]{10} [0-9:]{8}\\] test")
  expect_match(logger.debug("test"), "DEBUG \\[[0-9-]{10} [0-9:]{8}\\] test")
  expect_match(logger.trace("test"), "TRACE \\[[0-9-]{10} [0-9:]{8}\\] test")

})
