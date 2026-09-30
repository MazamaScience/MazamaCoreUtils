# Helper: write a small R script and return its path
.makeLintFile <- function(dir, name = "script.R") {
  path <- file.path(dir, name)
  writeLines(
    c(
      "fn_one(x = 1)",
      "fn_one(1)",
      "fn_two(foo = 1, bar = 2)",
      "fn_two(foo = 1)",
      "other_fn(x = 1)"
    ),
    path
  )
  path
}

rules <- list(
  fn_one = "x",
  fn_two = c("foo", "bar")
)

test_that("lintFunctionArgs_file() flags calls missing required arguments", {

  dir <- tempfile("lint")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE))
  path <- .makeLintFile(dir)

  result <- lintFunctionArgs_file(path, rules)

  expect_s3_class(result, "tbl_df")
  expect_named(
    result,
    c("file", "line_number", "column_number", "function_name",
      "named_args", "includes_required")
  )

  # Only functions named in the rules are reported
  expect_equal(nrow(result), 4)
  expect_false("other_fn" %in% result$function_name)

  expect_equal(result$line_number, 1:4)
  expect_equal(result$includes_required, c(TRUE, FALSE, TRUE, FALSE))
  expect_equal(unique(result$file), "script.R")

})

test_that("lintFunctionArgs_file() honors 'fullPath'", {

  dir <- tempfile("lint")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE))
  path <- .makeLintFile(dir)

  result <- lintFunctionArgs_file(path, rules, fullPath = TRUE)

  expect_equal(unique(result$file), normalizePath(path))

})

test_that("lintFunctionArgs_file() validates parameters", {

  dir <- tempfile("lint")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE))
  path <- .makeLintFile(dir)

  expect_error(lintFunctionArgs_file(rules = rules), "filePath")
  expect_error(lintFunctionArgs_file(path), "rules")
  expect_error(lintFunctionArgs_file(path, list("x")), "named list")
  expect_error(lintFunctionArgs_file(c(path, path), rules), "length 1")
  expect_error(lintFunctionArgs_file(dir, rules), "not a directory")

})

test_that("lintFunctionArgs_dir() lints all R files recursively", {

  dir <- tempfile("lint")
  dir.create(file.path(dir, "sub"), recursive = TRUE)
  on.exit(unlink(dir, recursive = TRUE))

  .makeLintFile(dir, "a.R")
  .makeLintFile(file.path(dir, "sub"), "b.R")
  writeLines("fn_one(1)", file.path(dir, "notes.txt"))

  result <- lintFunctionArgs_dir(dir, rules)

  expect_equal(nrow(result), 8)
  expect_setequal(unique(result$file), c("a.R", "b.R"))

})

test_that("lintFunctionArgs_dir() validates parameters", {

  dir <- tempfile("lint")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE))
  path <- .makeLintFile(dir)

  expect_error(lintFunctionArgs_dir(dir), "rules")
  expect_error(lintFunctionArgs_dir(dir, list("x")), "named list")
  expect_error(lintFunctionArgs_dir(c(dir, dir), rules), "length 1")
  expect_error(lintFunctionArgs_dir(path, rules), "directory")

})

test_that("timezoneLintRules is a named list of argument names", {

  expect_type(timezoneLintRules, "list")
  expect_false(is.null(names(timezoneLintRules)))
  expect_true(all(vapply(timezoneLintRules, is.character, logical(1))))
  expect_equal(timezoneLintRules[["as.POSIXct"]], "tz")
  expect_equal(timezoneLintRules[["Sys.time"]], "DEPRECATED")

})
