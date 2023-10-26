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
