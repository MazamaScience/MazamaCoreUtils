
#' @title Create one or more unique locationIDs
#' @description A unique locationID is created for each incoming
#' \code{longitude} and \code{latitude}.
#'
#' When the \code{"digest"} algorithm is specified,
#' the following code is used to generate each locationID.
#'
#' \preformatted{
#' # Retain accuracy up to ~.1m
#' locationString <- paste0(
#'   sprintf("\%.7f", longitude),
#'   "_",
#'   sprintf("\%.7f", latitude)
#' )
#' # Avoid collisions until billions of records
#' locationID <- digest::digest(locationString, algo = "xxhash64")
#' }
#'
#' When the \code{"geohash"} algorithm is specified,
#' the following code is used:
#'
#' \preformatted{
#'   locationID <-
#'     geohashTools::gh_encode(latitude, longitude, precision = 10)
#' }
#'
#' See the references for details on either algorithm.
#'
#' @note The \code{"geohash"} algorithm is preferred but the \code{"digest"}
#' algorithm is retained (and the default) because several existing databases
#' use the \code{"digest"} algorithm as a unique identifier.
#'
#' @param longitude Vector of longitudes in decimal degrees E.
#' @param latitude Vector of latitudes in decimal degrees N.
#' @param algorithm Algorithm to use -- either \code{"digest"} or \code{"geohash"}.
#' @return Vector of character locationIDs.
#' @examples
#' library(MazamaCoreUtils)
#'
#' # Wenatchee
#' lon <- -120.325278
#' lat <- 47.423333
#' locationID <- createLocationID(lon, lat)
#' @references \url{https://en.wikipedia.org/wiki/Decimal_degrees}
#' @references \url{https://www.johndcook.com/blog/2017/01/10/probability-of-secure-hash-collisions/}
#' @references \url{https://michaelchirico.github.io/geohashTools/index.html}
#' @rdname createLocationID
#' @export
#'
createLocationID <- function(
  longitude = NULL,
  latitude = NULL,
  algorithm = c("digest", "geohash")
) {

  # ----- Validate parameters --------------------------------------------------

  validateLonsLats(longitude, latitude)

  algorithm <- match.arg(algorithm)

  # ----- Create location hash -------------------------------------------------

  if ( algorithm == "digest" ) {

    # meters per decimal degree:
    #  https://en.wikipedia.org/wiki/Decimal_degrees

    # Retain accuracy up to ~.1m
    locationString <- paste0(
      sprintf("%.7f", longitude),
      "_",
      sprintf("%.7f", latitude)
    )

    # Explanation of collision frequency:
    #   https://www.johndcook.com/blog/2017/01/10/probability-of-secure-hash-collisions/
    #
    # > a hash function with range of size N can hash on the order of sqrt(N) values
    # > before running into collisions.
    #
    # A 32 bit hash will run into collisions at (2^32)^0.5 = 65,536
    # A 64 bit hash will run into collisions at (2^64)^0.5 = 4,294,967,296
    #
    # One can imagine a table with 60K known locations so it looks like a 32 bit
    # hash is not quite safe enough.

    # Use base::() mnapply to vectorise digest::digest()
    locationID <- mapply(
      function(x) { digest::digest(x, algo = "xxhash64") },
      locationString,
      SIMPLIFY = TRUE,
      USE.NAMES = FALSE
    )

  } else if ( algorithm == "geohash" ) {

    locationID <- mapply(
      function(lat, lon) { geohashTools::gh_encode(lat, lon, precision = 10) },
      latitude,
      longitude,
      SIMPLIFY = TRUE,
      USE.NAMES = FALSE
    )

  } else {

    stop(sprintf("algorithm = '%s' is not recognized", algorithm))

  }

  # ----- Return ---------------------------------------------------------------

  return(locationID)

}

#' @title Validate longitude and latitude vectors
#' @description Longitude and latitude vectors validated to be parseable as numeric
#' and within the bounds -180:180 and -90:90. If validation fails, an error is
#' generated.
#' @param longitude Vector of longitudes in decimal degrees E.
#' @param latitude Vector of latitudes in decimal degrees N.
#' @param na.rm Logical specifying whether to remove \code{NA} values before
#' validation.
#' @return Invisibly returns \code{TRUE} if no error message has been generated.
#' @rdname validateLonsLats
#' @export
#'
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
    stop("all latitudes must be a valid values between -90 and 90")

  return(invisible(TRUE))

}


#' @title Validate longitude and latitude values
#' @description Longitude and latitude are validated to be parseable as numeric
#' and within the bounds -180:180 and -90:90. If validation fails, an error is
#' generated.
#' @param longitude Single longitude in decimal degrees E.
#' @param latitude Single latitude in decimal degrees N.
#' @return Invisibly returns \code{TRUE} if no error message has been generated.
#' @rdname validateLonLat
#' @export
#'
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
    stop(paste0(
      "longitude and latitude must be single values"
    ))
  }

  if ( is.na(longitude) || longitude < -180 || longitude > 180 )
    stop("'longitude' must be a valid value between -180 and 180")

  if ( is.na(latitude) || latitude < -90 || latitude > 90 )
    stop("'latitude' must be a valid value between -90 and 90")

  return(invisible(TRUE))

}
