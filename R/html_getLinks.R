#' @name html_getLinks
#'
#' @importFrom rlang .data
#'
#' @title Find all links in an html page
#'
#' @param url URL or file path of an html page.
#' @param relative Logical instruction to return relative URLs.
#'
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
#' Wrapper functions \code{html_getLinkNames()} and \code{html_getLinkUrls()}
#' return the appropriate columns as vectors.
#'
#' @examples
#' library(MazamaCoreUtils)
#'
#' # Fail gracefully if the resource is not available
#' try({
#'
#'   # US Census 2019 shapefiles
#'   url <- "https://www2.census.gov/geo/tiger/GENZ2019/shp/"
#'
#'   # Extract links
#'   dataLinks <- html_getLinks(url)
#'
#'   dataLinks <- dataLinks %>%
#'     dplyr::filter(stringr::str_detect(linkName, "us_county"))
#'   head(dataLinks, 10)
#'
#' }, silent = FALSE)
#'
#' @rdname html_getLinks
#' @export

html_getLinks <- function(
  url = NULL,
  relative = TRUE
) {

  # ----- Validate parameters --------------------------------------------------

  MazamaCoreUtils::stopIfNull(url)

  if ( !is.logical(relative) )
    relative <- TRUE

  # ----- Extract the links ----------------------------------------------------

  try({

    urlAttributes <- xml2::read_html(url) %>% xml2::xml_find_all("//a[@href]")

    urlText <-
      urlAttributes %>%
      xml2::xml_text()

    urlLinks <-
      urlAttributes %>%
      xml2::xml_attr("href")

  }, silent = TRUE) %>%
  stopOnError()

  df <- dplyr::tibble(linkName = urlText, linkUrl = urlLinks)

  # ----- Filter URLs -------------------------------------------------

  df <-
    df %>%

    # Remove NA values
    dplyr::filter(!is.na(.data$linkUrl) & !is.na(.data$linkName)) %>%

    # Remove Apache indexing
    dplyr::filter(stringr::str_detect(.data$linkUrl, "^?C=.;O=.*", negate = TRUE)) %>%

    # Remove "Parent Directory"
    dplyr::filter(stringr::str_detect(.data$linkName, "Parent Directory", negate = TRUE)) %>%

    # Format URLs beginning with //
    dplyr::mutate(linkUrl = stringr::str_replace(.data$linkUrl, stringr::regex("^//"), ""))


  # ----- Handle relative URLs -------------------------------------------------

  if ( !relative ) {
    # Remove ending /
    if ( stringr::str_sub(url, -1) == "/" )
      url <- stringr::str_sub(url, 0, -2)

    # Append URL to records not beginning with http or www
    df <-
      df %>%
      dplyr::mutate(linkUrl = stringr::str_replace(.data$linkUrl, stringr::regex("^(?!http|www).*"), file.path(url, df$linkUrl)))
  }

  # ----- Return ---------------------------------------------------------------

  return(df)

}

#' @rdname html_getLinks
#' @param url URL or file path of an html page.
#' @export
html_getLinkNames <- function(
  url = NULL
) {

  # ----- Validate parameters --------------------------------------------------

  MazamaCoreUtils::stopIfNull(url)

  # ----- Extract the link text ------------------------------------------------

  linkNames <-
    html_getLinks(url) %>%
    dplyr::pull("linkName")

  # ----- Return ---------------------------------------------------------------

  return(linkNames)

}

#' @rdname html_getLinks
#' @param url URL or file path of an html page.
#' @param relative Logical instruction to return relative URLs.
#' @export
html_getLinkUrls <- function(
  url = NULL,
  relative = TRUE
) {

  # ----- Validate parameters --------------------------------------------------

  MazamaCoreUtils::stopIfNull(url)

  # ----- Extract the link text ------------------------------------------------

  linkUrls <-
    html_getLinks(url, relative) %>%
    dplyr::pull("linkUrl")

  # ----- Return ---------------------------------------------------------------

  return(linkUrls)

}
