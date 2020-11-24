#' @name html_getTables
#'
#' @importFrom rlang .data
#'
#' @title Find all tables in an html page
#'
#' @param url URL or file path of an html page.
#' @param header Use first row as header? If NA, will use first row if it
#' consists of <th> tags.
#'
#' @return A list of dataframes representing each table on a html page.
#'
#' @description Parses an html page to extract all \code{<table>} elements and
#' return them in a list of dataframes representing each table. The columns and
#' rows of these dataframes are that of the table it represents. A single table
#' can be extracted as a dataframe by passing the index of the table in addition
#' to the url to \code{html_getTable()}.
#'
#' @examples
#' library(MazamaCoreUtils)
#'
#' # Wikipedia's list of timezones
#' url <- "http://en.wikipedia.org/wiki/List_of_tz_database_time_zones"
#'
#' # Extract tables
#' tables <- html_getTables(url)
#'
#' # Extract the first table
#' # NOTE: Analogous to firstTable <- html_getTable(url, index = 1)
#' firstTable <- tables[[1]]
#'
#' head(firstTable)
#' nrow(firstTable)
#'
#' @rdname html_getTables
#' @export

html_getTables <- function(
  url = NULL,
  header = NA
) {

  # ----- Validate parameters --------------------------------------------------

  MazamaCoreUtils::stopIfNull(url)

  # ----- Extract the tables ----------------------------------------------------

  result <- try({

    # Get the raw HTML from the URL
    urlXML <- xml2::read_html(url)

    # Get a list of tables in the document
    tableNodes <- rvest::html_nodes(urlXML, css = "table")

    # Make this list human-readable
    tables_clean <- rvest::html_table(
      tableNodes,
      header = header,        # use first row as header
      trim = TRUE,            # remove leading/trailing white space
      fill = TRUE,            # NA fill rows with fewer than max columns
      dec = "."
    )

  }, silent = TRUE)
  stopOnError(result)

  # ----- Return ---------------------------------------------------------------

  return(tables_clean)

}

#' @rdname html_getTables
#' @param url URL or file path of an html page.
#' @param header Use first row as header? If NA, will use first row if it
#' consists of <th> tags.
#' @param index Index identifying which table to to return.
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

  result <- try({

    # Get a list of tables in this document
    tables <- html_getTables(url, header = header)

    returnTable <- tables[[index]]

  }, silent = TRUE)
  stopOnError(result)

  # ----- Return ---------------------------------------------------------------

  return(returnTable)

}


