#' Create a mask of valid locations
#'
#' Create a logical mask identifying valid longitude/latitude pairs.
#'
#' The returned logical vector contains `TRUE` for valid locations and `FALSE`
#' for invalid locations. This is useful for filtering data frames to retain
#' only records with valid geographic coordinates.
#'
#' Longitude and latitude values are considered valid when they:
#'
#' \itemize{
#'   \item fall within `lonRange` and `latRange`
#'   \item are not missing
#'   \item are not located at `(0, 0)` when `removeZeroZero = TRUE`
#' }
#'
#' The `lonRange` and `latRange` arguments can be used to restrict valid
#' locations to a rectangular geographic region.
#'
#' @param longitude Vector of longitudes in decimal degrees east.
#' @param latitude Vector of latitudes in decimal degrees north.
#' @param lonRange Range of valid longitudes.
#' @param latRange Range of valid latitudes.
#' @param removeZeroZero Logical specifying whether the coordinate pair
#'   `(0, 0)` should be treated as invalid.
#'
#' @return
#' Logical vector identifying valid locations.
#'
#' @examples
#' createLocationMask(
#'   longitude = c(-120, NA, -120, -220, -120, 0),
#'   latitude = c(45, 45, NA, 45, 100, 0)
#' )
#'
#' createLocationMask(
#'   longitude = -120:-90,
#'   latitude = 20:50,
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
    longitude >= lonRange[1] &
    longitude <= lonRange[2] &
    latitude >= latRange[1] &
    latitude <= latRange[2]

  mask[is.na(mask)] <- FALSE

  # ----- Return ---------------------------------------------------------------

  return(mask)

}

