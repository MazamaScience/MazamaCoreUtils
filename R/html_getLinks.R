#' @export
#'
#' @title Find all links in an html page
#'
#' @param url URL or file path of an html page.
#'
#' @return A dataframe with \code{linkName} and \code{linkUrl} columns.
#'
#' @description Parses an html page to extract all \code{<a href="...">...</a>}
#' links and return them in a dataframe where \code{linkName} is the human
#' readable name and \code{linkUrl} is the \code{href} portion.
#'
#' This is especially useful for extracting data from an index page that shows
#' the contents of a web accessible directory.
#'
#' @examples
#' \donttest{
#' library(MazamaCoreUtils)
#'
#' # US Census 2019 shapefiles
#' dataLinks <- html_getLinks("https://www2.census.gov/geo/tiger/GENZ2019/shp/")
#'
#' dataLinks %>%
#'   dplyr::filter(stringr::str_detect(linkName, "us_county"))
#' }

html_getLinks <- function(
  url = NULL
) {

  # ----- Validate parameters --------------------------------------------------

  MazamaCoreUtils::stopIfNull(url)

  # ----- Extract the links ----------------------------------------------------

  url_links <- html_getLinkUrls(url)
  url_text <- html_getLinkNames(url)

  df <- data.frame(linkUrl = url_links, linkName = url_text)

  # ----- Return ---------------------------------------------------------------

  return(df)

}

html_getLinkNames <- function(
  url = NULL
) {

  # ----- Validate parameters --------------------------------------------------

  MazamaCoreUtils::stopIfNull(url)

  # ----- Extract the link text ------------------------------------------------

  result <- try({

    text <-
      xml2::read_html(url) %>%
      xml2::xml_find_all("//a[@href]") %>%
      xml2::xml_text()

  }, silent = TRUE)
  stopOnError(result)
  result <- result[!is.na(result)]

  # ----- Return ---------------------------------------------------------------

  return(result)

}

html_getLinkUrls <- function(
  url = NULL
) {

  # ----- Validate parameters --------------------------------------------------

  MazamaCoreUtils::stopIfNull(url)

  # ----- Extract the link urls ------------------------------------------------

  result <- try({

    links <-
      xml2::read_html(url) %>%
      xml2::xml_find_all("//a") %>%
      xml2::xml_attr("href")

  }, silent = TRUE)
  stopOnError(result)
  result <- result[!is.na(result)]

  # ----- Return ---------------------------------------------------------------

  return(result)

}
