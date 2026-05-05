#' Extract tables from an HTML page
#'
#' Parse an HTML page and return all `<table>` elements as a list of data
#' frames.
#'
#' The `url` argument may be either a remote URL or a local file path. Tables are
#' parsed with [rvest::html_table()]. To extract a single table, use
#' [html_getTable()].
#'
#' @param url URL or local file path of an HTML page.
#' @param header Logical specifying whether the first row should be used as
#'   column names. If `NA`, the first row is used only when it contains `<th>`
#'   elements.
#'
#' @return
#' List of data frames, one for each HTML table.
#'
#' @examples
#' \dontrun{
#' url <- "https://en.wikipedia.org/wiki/List_of_tz_database_time_zones"
#'
#' tables <- html_getTables(url)
#' firstTable <- tables[[1]]
#'
#' head(firstTable)
#' nrow(firstTable)
#' }
#'
#' @name html_getTables
#' @rdname html_getTables
#' @export

html_getTables <- function(
  url = NULL,
  header = NA
) {

  # ----- Validate parameters --------------------------------------------------

  MazamaCoreUtils::stopIfNull(url)

  # ----- Extract the tables ----------------------------------------------------

  try({

    # Get the raw HTML from the URL
    urlXML <- xml2::read_html(url)

    # Get a list of tables in the document
    tableNodes <- rvest::html_elements(urlXML, css = "table")

    # Make this list human-readable
    tables_clean <- rvest::html_table(
      tableNodes,
      header = header,        # use first row as header
      trim = TRUE,            # remove leading/trailing white space
      fill = TRUE,            # NA fill rows with fewer than max columns
      dec = "."
    )

  }, silent = TRUE) %>%
  stopOnError()

  # ----- Return ---------------------------------------------------------------

  return(tables_clean)

}

#' @rdname html_getTables
#'
#' @param index Index identifying which table to return.
#'
#' @return
#' A single data frame containing the requested HTML table.
#'
#' @export
html_getTable <- function(
  url = NULL,
  header = NA,
  index = 1
) {

  # ----- Validate parameters --------------------------------------------------

  MazamaCoreUtils::stopIfNull(url)

  index <- as.integer(index)
  if ( index < 1 )
    index <- 1

  # ----- Extract the table ----------------------------------------------------

  try({

    # Get a list of tables in this document
    tables <- html_getTables(url, header = header)

    returnTable <- tables[[index]]

  }, silent = TRUE) %>%
  stopOnError()

  # ----- Return ---------------------------------------------------------------

  return(returnTable)

}


