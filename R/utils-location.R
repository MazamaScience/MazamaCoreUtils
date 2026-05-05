#' Validate longitude and latitude vectors
#'
#' Validate longitude and latitude vectors to ensure they are numeric,
#' have matching lengths, and contain values within valid geographic bounds.
#'
#' Longitudes must fall between -180 and 180 degrees and latitudes must
#' fall between -90 and 90 degrees. If validation fails, an error is
#' generated.
#'
#' @param longitude Vector of longitudes in decimal degrees east.
#' @param latitude Vector of latitudes in decimal degrees north.
#' @param na.rm Logical specifying whether to remove `NA` values before
#'   validation.
#'
#' @return
#' Invisibly returns `TRUE` if validation succeeds.
#'
#' @examples
#' longitude <- c(-122.5, -122.4)
#' latitude <- c(47.5, 47.6)
#'
#' validateLonsLats(longitude, latitude)
#'
#' # Remove missing values before validation
#' validateLonsLats(
#'   c(-122.5, NA),
#'   c(47.5, NA),
#'   na.rm = TRUE
#' )
#'
#' \dontrun{
#' validateLonsLats(c(-200, 0), c(45, 46))
#' }
#'
#' @rdname validateLonsLats
#' @export
validateLonsLats <- function(
    longitude = NULL,
    latitude = NULL,
    na.rm = FALSE
) {

  # ----- Validate parameters --------------------------------------------------

  MazamaCoreUtils::stopIfNull(longitude)
  MazamaCoreUtils::stopIfNull(latitude)

  if ( !is.numeric(longitude) )
    stop("'longitude' must be numeric")

  if ( !is.numeric(latitude) )
    stop("'latitude' must be numeric")

  if ( length(longitude) != length(latitude) )
    stop("'longitude' and 'latitude' must have the same length")

  # Remove locations with NAs
  if ( na.rm ) {
    good_mask <- !is.na(longitude) & !is.na(latitude)
    longitude <- longitude[good_mask]
    latitude <- latitude[good_mask]
  }

  if ( anyNA(longitude) || any(longitude < -180) || any(longitude > 180 ))
    stop("all longitudes must be valid values between -180 and 180")

  if ( anyNA(latitude) || any(latitude < -90) || any(latitude > 90) )
    stop("all latitudes must be valid values between -90 and 90")

  return(invisible(TRUE))

}


#' Validate longitude and latitude values
#'
#' Validate a single longitude/latitude pair to ensure both values are
#' numeric scalars and fall within valid geographic bounds.
#'
#' Longitudes must fall between -180 and 180 degrees and latitudes must
#' fall between -90 and 90 degrees. If validation fails, an error is
#' generated.
#'
#' @param longitude Single longitude in decimal degrees east.
#' @param latitude Single latitude in decimal degrees north.
#'
#' @return
#' Invisibly returns `TRUE` if validation succeeds.
#'
#' @examples
#' validateLonLat(-122.5, 47.5)
#'
#' \dontrun{
#' validateLonLat(-200, 47.5)
#' validateLonLat(-122.5, NA)
#' }
#'
#' @rdname validateLonLat
#' @export
validateLonLat <- function(
    longitude = NULL,
    latitude = NULL
) {

  MazamaCoreUtils::stopIfNull(longitude)
  MazamaCoreUtils::stopIfNull(latitude)

  if ( !is.numeric(longitude) )
    stop("'longitude' must be numeric")

  if ( !is.numeric(latitude) )
    stop("'latitude' must be numeric")

  if ( length(longitude) > 1 || length(latitude) > 1 ) {
    stop("longitude and latitude must be single values")
  }

  if ( is.na(longitude) || longitude < -180 || longitude > 180 )
    stop("'longitude' must be a valid value between -180 and 180")

  if ( is.na(latitude) || latitude < -90 || latitude > 90 )
    stop("'latitude' must be a valid value between -90 and 90")

  return(invisible(TRUE))

}
