# Helper: write a small HTML page and return its path
.makeLinkFile <- function() {
  htmlFile <- tempfile(fileext = ".html")
  writeLines(
    paste0(
      "<html><body>",
      "<a href=\"?C=N;O=D\">Name</a>",
      "<a href=\"/parent/\">Parent Directory</a>",
      "<a href=\"file1.csv\">File One</a>",
      "<a href=\"sub/file2.csv\">File Two</a>",
      "<a href=\"//example.com/file3.csv\">File Three</a>",
      "<a name=\"anchor\">No href</a>",
      "</body></html>"
    ),
    htmlFile
  )
  htmlFile
}

test_that("html_getLinks() returns links and filters index noise", {

  htmlFile <- .makeLinkFile()
  on.exit(unlink(htmlFile))

  links <- html_getLinks(htmlFile)

  expect_s3_class(links, "tbl_df")
  expect_named(links, c("linkName", "linkUrl"))

  # Apache sort links, "Parent Directory" and anchors without href are removed
  expect_equal(links$linkName, c("File One", "File Two", "File Three"))

  # Leading "//" is removed when relative = TRUE (the default)
  expect_equal(
    links$linkUrl,
    c("file1.csv", "sub/file2.csv", "example.com/file3.csv")
  )

})

test_that("html_getLinkNames() and html_getLinkUrls() return vectors", {

  htmlFile <- .makeLinkFile()
  on.exit(unlink(htmlFile))

  expect_equal(html_getLinkNames(htmlFile), c("File One", "File Two", "File Three"))
  expect_equal(
    html_getLinkUrls(htmlFile),
    c("file1.csv", "sub/file2.csv", "example.com/file3.csv")
  )

})

test_that("html_getLinks() handles missing input and missing files", {

  expect_error(html_getLinks(), "url")
  expect_error(html_getLinks(file.path(tempdir(), "no_such_file.html")))

})

test_that(".formatLinkUrls() handles relative and absolute output", {

  urls <- c("file1.csv", "sub/file2.csv", "//example.com/file3.csv", "https://other.org/f4.csv")
  base <- "https://host.org/dir/index.html"

  expect_equal(
    .formatLinkUrls(urls, base, relative = TRUE),
    c("file1.csv", "sub/file2.csv", "example.com/file3.csv", "https://other.org/f4.csv")
  )

  # Protocol-relative URLs must pick up the scheme of the base URL
  expect_equal(
    .formatLinkUrls(urls, base, relative = FALSE),
    c(
      "https://host.org/dir/file1.csv",
      "https://host.org/dir/sub/file2.csv",
      "https://example.com/file3.csv",
      "https://other.org/f4.csv"
    )
  )

})
