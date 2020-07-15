#' @name html_getLinks
#'
#' @title Find all links in an html page
#'
#' @param url URL or file path of an html page.
#' @param relative Logical instruction to return relative URLs.
#' @return A dataframe with \code{linkName} and/or \code{linkUrl} columns.
#'
#' @description Parses an html page to extract all \code{<a href="...">...</a>}
#' links and return them in a dataframe where \code{linkName} is the human
#' readable name and \code{linkUrl} is the \code{href} portion. By default this
#' function will return relative URLs.
#'
#' This is especially useful for extracting data from an index page that shows
#' the contents of a web accessible directory.
#'
#' @examples
#' # US Census 2019 shapefiles
#' dataLinks <- html_getLinks("https://www2.census.gov/geo/tiger/GENZ2019/shp/")
#'
#' dataLinks %>%
#'   dplyr::filter(stringr::str_detect(linkName, "us_county"))
#' head(dataLinks, 10)
#'
#' @rdname html_getLinks
#' @export

html_getLinks <- function(
  url = NULL,
  relative = TRUE
) {

  # ----- Validate parameters --------------------------------------------------

  MazamaCoreUtils::stopIfNull(url)

  # Remove ending /
  if (str_sub(url, -1) == "/") {
    url <- str_sub(url, 0, -2)
  }

  # ----- Extract the links ----------------------------------------------------

  result <- try({

    urlXML <- xml2::read_html(url)
    url_text <-
      urlXML %>%
      xml2::xml_find_all("//a[@href]") %>%
      xml2::xml_text()
    url_links <-
      urlXML %>%
      xml2::xml_find_all("//a") %>%
      xml2::xml_attr("href")

  }, silent = TRUE)
  stopOnError(result)

  url_text <- url_text[!is.na(url_text)]
  url_links <- url_links[!is.na(url_links)]

  df <- data.frame(linkName = url_text, linkUrl = url_links)

  # ----- Filter URLs -------------------------------------------------

  df <-
    df %>%

    # Remove Apache indexing
    dplyr::filter(stringr::str_detect(linkUrl, "^?C=.;O=.*", negate = TRUE)) %>%

    # Replace URLs beginning with //
    dplyr::mutate(linkUrl = linkUrl %>% stringr::str_replace(regex("^//"), ""))


  # ----- Handle relative URLs -------------------------------------------------

  if (!relative) {
    df <-
      df %>%
      # Append URL to records not beginning with http or www
      dplyr::mutate(linkUrl = linkUrl %>% stringr::str_replace(regex("^(?!http|www).*"), file.path(url, linkUrl)))
  }
  # ----- Return ---------------------------------------------------------------

  return(df)

}

html_getLinkNames <- function(
  url = NULL,
  relative = TRUE
) {

  # ----- Validate parameters --------------------------------------------------

  MazamaCoreUtils::stopIfNull(url)

  # ----- Extract the link text ------------------------------------------------

  linkData <- html_getLinks(url, relative)
  linkNames <- dplyr::pull(linkData, linkName)

  # ----- Return ---------------------------------------------------------------

  return(linkNames)

}

html_getLinkUrls <- function(
  url = NULL,
  relative = TRUE
) {

  # ----- Validate parameters --------------------------------------------------

  MazamaCoreUtils::stopIfNull(url)

  # ----- Extract the link text ------------------------------------------------

  linkData <- html_getLinks(url, relative)
  linkUrls <- dplyr::pull(linkData, linkUrl)

  # ----- Return ---------------------------------------------------------------

  return(linkUrls)

}
