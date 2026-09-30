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
