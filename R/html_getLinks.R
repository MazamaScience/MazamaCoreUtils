#' Extract links from an HTML page
#'
#' Parse an HTML page and return all `<a href="...">...</a>` links as a data
#' frame.
#'
#' The returned data frame contains the human-readable link text in `linkName`
#' and the `href` value in `linkUrl`. This is useful for extracting links from
#' index pages, including web-accessible directories that list downloadable
#' files.
#'
#' Wrapper functions [html_getLinkNames()] and [html_getLinkUrls()] return the
#' corresponding columns as character vectors.
#'
#' @param url URL or local file path of an HTML page.
#' @param relative Logical specifying whether to return relative URLs. If
#'   `FALSE`, relative URLs are converted to absolute URLs using `url` as the
#'   base.
#'
#' @return
#' A tibble with `linkName` and `linkUrl` columns.
#'
#' @examples
#' \dontrun{
#'
#' # If you want to download lots of USCensus shapefiles
#' url <- "https://www2.census.gov/geo/tiger/GENZ2019/shp/"
#'
#' browseURL(url)
#'
#' dataLinks <- html_getLinks(url)
#'
#' dataLinks <-
#'   dataLinks %>%
#'   dplyr::filter(stringr::str_detect(linkName, "us_county"))
#'
#' head(dataLinks, 10)
#'
#' html_getLinkNames(url)
#' html_getLinkUrls(url, relative = FALSE)
#' }
#'
#' @name html_getLinks
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
    dplyr::filter(stringr::str_detect(.data$linkUrl, "^\\?C=.;O=.*", negate = TRUE)) %>%

    # Remove "Parent Directory"
    dplyr::filter(stringr::str_detect(.data$linkName, "Parent Directory", negate = TRUE)) %>%

    # Format URLs beginning with //
    dplyr::mutate(linkUrl = stringr::str_replace(.data$linkUrl, stringr::regex("^//"), ""))


  # ----- Expand relative URLs -------------------------------------------------

  if ( !relative ) {

    df <-
      df %>%
      dplyr::mutate(
        linkUrl = xml2::url_absolute(.data$linkUrl, base = url)
      )

  }

  # ----- Return ---------------------------------------------------------------

  return(df)

}

#' @rdname html_getLinks
#'
#' @return
#' `html_getLinkNames()` returns a character vector of link names.
#'
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
#'
#' @return
#' `html_getLinkUrls()` returns a character vector of link URLs.
#'
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
