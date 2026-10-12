test_that("html_getTable() handles 'index' values", {

  htmlFile <- tempfile(fileext = ".html")
  writeLines(
    "<html><body><table><tr><th>a</th></tr><tr><td>1</td></tr></table></body></html>",
    htmlFile
  )
  on.exit(unlink(htmlFile))

  expect_s3_class(html_getTable(htmlFile, index = 1), "data.frame")
  expect_warning(html_getTable(htmlFile, index = 0), "index")
  expect_error(html_getTable(htmlFile, index = NA), "index")

})

test_that("html_getTables() returns all tables in a local file", {

  htmlFile <- tempfile(fileext = ".html")
  writeLines(
    paste0(
      "<html><body>",
      "<table><tr><th>a</th><th>b</th></tr><tr><td>1</td><td>2</td></tr></table>",
      "<table><tr><th>c</th></tr><tr><td>3</td></tr><tr><td>4</td></tr></table>",
      "</body></html>"
    ),
    htmlFile
  )
  on.exit(unlink(htmlFile))

  tables <- html_getTables(htmlFile)

  expect_type(tables, "list")
  expect_length(tables, 2)
  expect_named(tables[[1]], c("a", "b"))
  expect_equal(nrow(tables[[2]]), 2)

  # html_getTable() selects by index
  expect_named(html_getTable(htmlFile, index = 2), "c")

  # index beyond the number of tables is an error
  expect_error(html_getTable(htmlFile, index = 3))

})

test_that("html_getTables() handles missing input and missing files", {

  expect_error(html_getTables(), "url")
  expect_error(html_getTables(file.path(tempdir(), "no_such_file.html")))

})

test_that("html_getTables() fills ragged rows with NA", {

  htmlFile <- tempfile(fileext = ".html")
  writeLines(
    paste0(
      "<html><body><table>",
      "<tr><th>a</th><th>b</th><th>c</th></tr>",
      "<tr><td>1</td><td>2</td></tr>",
      "<tr><td>3</td><td>4</td><td>5</td></tr>",
      "</table></body></html>"
    ),
    htmlFile
  )
  on.exit(unlink(htmlFile))

  expect_no_warning(table <- html_getTable(htmlFile))

  expect_named(table, c("a", "b", "c"))
  expect_equal(nrow(table), 2)
  expect_true(is.na(table$c[1]))
  expect_equal(table$c[2], 5)

})
