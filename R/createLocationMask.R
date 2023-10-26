#' @title Create a mask of valid locations
#' @description A logical vector is created with either \code{TRUE} or
#' \code{FALSE} for each incoming \code{longitude, latitude} pair with
#' \code{TRUE} indicating a valid location. This can be used to filter dataframes
#' to retain only records with valid locations.
#'
#' \code{lonRange} and \code{latRange} can be used to create a valid-mask for
#' locations within a rectangular area.
#'
#' \code{removeZeroZero} will invalidate the location \code{0.0, 0.0} which is
#' sometimes seen in poorly QC'ed datasets.
#'
#' \code{NA} values found in \code{longitude} or \code{latitude} will result
#' in a mask value of \code{FALSE}.
#'
#' @param longitude Vector of longitudes in decimal degrees E.
#' @param latitude Vector of latitudes in decimal degrees N.
#' @param lonRange Range of valid longitudes.
#' @param latRange Range of valid latitudes.
#' @param removeZeroZero Logical indicating whether locations at \code{0.0, 0.0}
#' should be marked as invalid.
#'
#' @return Vector of logical values.
#'
#' @examples
#' library(MazamaCoreUtils)
#'
#' createLocationMask(
#'   longitude = c(-120, NA, -120, -220, -120, 0),
#'   latitude = c(45, 45, NA, 45, 100, 0)
#' )
#'
#' createLocationMask(
#'   longitude = c(-120:-90),
#'   latitude = c(20:50),
#'   lonRange = c(-110, -100),
#'   latRange = c(30, 40)
#' )
#'
#' @rdname createLocationMask
#' @export
#'
createLocationMask <- function(
    longitude = NULL,
    latitude = NULL,
    lonRange = c(-180, 180),
    latRange = c(-90, 90),
    removeZeroZero = TRUE
) {

  # ----- Validate parameters --------------------------------------------------

  MazamaCoreUtils::stopIfNull(longitude)
  MazamaCoreUtils::stopIfNull(latitude)
  MazamaCoreUtils::stopIfNull(lonRange)
  MazamaCoreUtils::stopIfNull(latRange)
  MazamaCoreUtils::stopIfNull(removeZeroZero)

  suppressWarnings({
    longitude <- as.numeric(longitude)
    latitude <- as.numeric(latitude)
  })

  if ( length(longitude) != length(latitude) )
    stop("longitude and latitude are not the same size.")

  if ( any(lonRange < -180) || any(lonRange > 180) )
    stop("lonRange includes invalid longitudes")

  if ( any(latRange < -90) || any(latRange > 90) )
    stop("latRange includes invalid latitudes")

  # ----- Build mask -----------------------------------------------------------

  zero_mask <- rep(TRUE, times = length(longitude))
  if ( removeZeroZero )
    zero_mask <- (longitude + latitude) != 0.0

  mask <-
    zero_mask &
    longitude > lonRange[1] &
    longitude < lonRange[2] &
    latitude > latRange[1] &
    latitude < latRange[2]

  mask[is.na(mask)] <- FALSE

  # ----- Return ---------------------------------------------------------------

  return(mask)

}

