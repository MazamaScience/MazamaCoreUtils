#' @name html_getTables
#'
#' @importFrom rlang .data
#'
#' @title Find all tables in an html page
#'
#' @param url URL or file path of an html page.
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
  url = NULL
) {

  # ----- Validate parameters --------------------------------------------------

  MazamaCoreUtils::stopIfNull(url)

  # ----- Extract the tables ----------------------------------------------------

  result <- try({

    # Get the raw HTML from the URL
    urlXML <- xml2::read_html(url)

    # Get a list of tables in the document
    tables <- rvest::html_nodes(urlXML, "table")

    # Make this list human-readable
    tables_clean <- rvest::html_table(tables, fill = TRUE)

  }, silent = TRUE)
  stopOnError(result)

  # ----- Return ---------------------------------------------------------------

  return(tables_clean)

}

#' @rdname html_getTables
#' @param url URL or file path of an html page.
#' @param index Index identifying which table to to return.
#' @export
html_getTable <- function(
  url = NULL,
  index = 1
) {

  # ----- Validate parameters --------------------------------------------------

  MazamaCoreUtils::stopIfNull(url)

  if ( !is.integer(index) || integer < 1 )
    index <- 1

  # ----- Extract the table ----------------------------------------------------

  result <- try({

    # Get a list of tables in this document
    tables <- html_getTables(url)

    returnTable <- tables[[index]]

  }, silent = TRUE)
  stopOnError(result)

  # ----- Return ---------------------------------------------------------------

  return(returnTable)

}


