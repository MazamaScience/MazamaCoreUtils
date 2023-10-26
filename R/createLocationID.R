#' @title Create one or more unique locationIDs
#' @description A unique locationID is created for each incoming
#' \code{longitude} and \code{latitude}.
#'
#' When the \code{"geohash"} algorithm is specified,
#' the following code is used:
#'
#' \preformatted{
#'   locationID <-
#'     geohashTools::gh_encode(latitude, longitude, precision = 10)
#' }
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
#' See the references for details on either algorithm.
#'
#' The locationID for any invalid location can be specified by the user with
#' the \code{invalidID} argument.
#'
#' @note The \code{"geohash"} algorithm is preferred but the \code{"digest"}
#' algorithm is retained because several existing databases
#' use the \code{"digest"} algorithm as a unique identifier.
#'
#' @param longitude Vector of longitudes in decimal degrees E.
#' @param latitude Vector of latitudes in decimal degrees N.
#' @param algorithm Algorithm to use -- either \code{"geohash"} or \code{"digest"}.
#' @param invalidID Identifier to use for invalid locations. This can be a
#' character string or \code{NA}.
#' @return Vector of character locationIDs.
#' @examples
#' library(MazamaCoreUtils)
#'
#' longitude <- c(-122.5, 0, NA, -122.5, -122.5)
#' latitude <- c( 47.5, 0, 47.5,   NA, 47.5)
#'
#' createLocationID(longitude, latitude)
#' createLocationID(longitude, latitude, invalidID = "bad")
#' createLocationID(longitude, latitude, algorithm = "digest")
#'
#' @references \url{https://en.wikipedia.org/wiki/Decimal_degrees}
#' @references \url{https://www.johndcook.com/blog/2017/01/10/probability-of-secure-hash-collisions/}
#' @references \url{https://michaelchirico.github.io/geohashTools/index.html}
#' @rdname createLocationID
#' @export
#'
createLocationID <- function(
    longitude = NULL,
    latitude = NULL,
    algorithm = c("geohash", "digest"),
    invalidID = as.character(NA)
) {

  # ----- Validate parameters --------------------------------------------------

  algorithm <- match.arg(algorithm)

  suppressWarnings({
    longitude <- as.numeric(longitude)
    latitude <- as.numeric(latitude)
  })

  # ----- Create locationID ----------------------------------------------------

  locationID <- rep(as.character(invalidID), times = length(longitude))
  mask <- createLocationMask(longitude, latitude)

  # If all locations are bad, return immediately
  if ( sum(mask) == 0 ) return(locationID)

  # Some valid locations, so continue

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
    locationID[mask] <- mapply(
      function(x) { digest::digest(x, algo = "xxhash64") },
      locationString[mask],
      SIMPLIFY = TRUE,
      USE.NAMES = FALSE
    )

  } else if ( algorithm == "geohash" ) {

    locationID[mask] <- mapply(
      function(lat, lon) { geohashTools::gh_encode(lat, lon, precision = 10) },
      latitude[mask],
      longitude[mask],
      SIMPLIFY = TRUE,
      USE.NAMES = FALSE
    )

  } else {

    stop(sprintf("algorithm = '%s' is not recognized", algorithm))

  }

  # ----- Return ---------------------------------------------------------------

  return(locationID)

}

