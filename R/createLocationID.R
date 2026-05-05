#' Create one or more unique location IDs
#'
#' Create a location ID for each longitude/latitude pair using a geohash.
#'
#' Each location ID is unique within a geohash grid cell. The `precision`
#' argument determines the size of the grid cell. At the equator, approximate
#' grid cell widths are:
#'
#' \preformatted{
#' precision   maximum grid cell width
#'         5   ~ 4.9 km
#'         6   ~ 1.2 km
#'         7   ~ 153 m
#'         8   ~ 38 m
#'         9   ~ 4.8 m
#'        10   ~ 1.2 m
#' }
#'
#' Invalid locations are assigned the value specified by `invalidID`, typically
#' `NA`.
#'
#' @param longitude Vector of longitudes in decimal degrees east.
#' @param latitude Vector of latitudes in decimal degrees north.
#' @param precision Precision used when encoding geohashes.
#' @param invalidID Identifier to use for invalid locations. This can be a
#'   character string or `NA`.
#'
#' @return
#' Character vector of location IDs.
#'
#' @examples
#' longitude <- c(-122.5, 0, NA, -122.5, -122.5)
#' latitude <- c(47.5, 0, 47.5, NA, 47.5)
#'
#' createLocationID(longitude, latitude)
#' createLocationID(longitude, latitude, precision = 7)
#' createLocationID(longitude, latitude, invalidID = "bad")
#'
#' @references
#' <https://michaelchirico.github.io/geohashTools/index.html>
#'
#' @export
createLocationID <- function(
    longitude = NULL,
    latitude = NULL,
    precision = 10,
    invalidID = as.character(NA)
) {

  # Validate parameters --------------------------------------------------------

  stopIfNull(longitude)
  stopIfNull(latitude)

  precision <- setIfNull(precision, 10)

  suppressWarnings({
    longitude <- as.numeric(longitude)
    latitude <- as.numeric(latitude)
  })


  # Create locationID ----------------------------------------------------------

  locationID <- rep(as.character(invalidID), times = length(longitude))
  mask <- createLocationMask(longitude, latitude, removeZeroZero = FALSE)

  # If all locations are bad, return immediately.
  if ( sum(mask) == 0 )
    return(locationID)

  locationID[mask] <- mapply(
    function(lat, lon) {
      geohashTools::gh_encode(lat, lon, precision)
    },
    latitude[mask],
    longitude[mask],
    SIMPLIFY = TRUE,
    USE.NAMES = FALSE
  )


  # Return ---------------------------------------------------------------------

  return(locationID)

}
