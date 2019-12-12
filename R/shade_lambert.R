#' @title Function for calculation of lambert shading (also known as hillshade in GIS) on raster data
#'
#' @description This function is just a wrapper around \link[rayshader]{lamb_shade} that supports
#' directly using \linkS4class{Raster} in the function. The function also tries to set
#' "reasonable" default values for parameters.
#'
#' @param surface_raster Object of class \linkS4class{Raster}.
#' @param date Date specified as \code{character} in form \code{YYYY-MM-DD}. Default value is
#' \code{\link{Sys.Date}}.
#' @param time Either character representation of time (in format \code{HH:MM:SS}),
#' or \code{\link[hms]{hms}} class, or one of text values:
#' \code{"noon", "sunrise", "sunset", "goldenHour", "goldenHourEnd"}. The textual representation is
#' used to determine sun elevation using function \code{\link[suncalc]{getSunlightPosition}}. Value
#' \code{"sunrise"} represents end of sunrise, while \code{"sunset"} represents start of sunset.
#' Default value is \code{"noon"}.
#' @param tzone a character string that specifies which time zone to parse the date with.
#' The string must be a time zone that is recognized by the user's OS. If no time zone is given the default
#' value \code{"UTC"} is used. Default value \code{"UTC"}.
#' @param only_facing_sun Boolean value specifing if the value should be determined for all parts of
#' the surface or only for parts oriented towards sun. Default value is FALSE, which means that it
#' is calculated for all parts of the surface.
#' @param z_value numeric value indicating the scale between xy and z. Typical use is if the distance
#' is measured in meters and elevation in foots, in such case the z value needs to be recalculated
#' to meters. Default value is \code{1}, which means that both measures use the same units.
#' @param verbose Should informative message be printed? Default \code{FALSE}.
#'
#' @return Object of class \linkS4class{Raster}.
#'
#' @export
#'
#' @name shade_lambert
#'
#' @importFrom suncalc getSunlightPosition
#' @importFrom lubridate ymd_hms with_tz
#'
shade_lambert_date_time <- function(surface_raster,
                                    date = as.character(Sys.Date()),
                                    time = "noon",
                                    tzone = "UTC",
                                    only_facing_sun = FALSE,
                                    z_value = 1,
                                    verbose = FALSE) {


  .check_raster(surface_raster)

  date <- .check_return_date(date)

  if (time %in% names(.sun_position_types())) {
    time <- .sun_position_types()[time]
  } else {
    time <- .check_return_time(time)
  }

  .check_TZ(tzone)

  # determine midpoint of the raster as long lat coordinates for further calculations of sun position
  mid_point_long_lat <- .get_midpoint_coordinates(surface_raster)

  # calculate time for sun elevation
  if (is.character(time)) {
    time <- .get_time_event(time, date, mid_point_long_lat[,2], mid_point_long_lat[,1])
  }

  # determine sun position
  date_time <- lubridate::ymd_hms(paste(date, time), tz = tzone)

  sun_pos <- suncalc::getSunlightPosition(date = lubridate::with_tz(date_time, tzone = "UTC"),
                                          lat = mid_point_long_lat[,2], lon = mid_point_long_lat[,1])

  sun_elevation <- sun_pos$altitude * (180/pi)

  sun_azimuth <- sun_pos$azimuth * (180/pi)

  sun_azimuth <- ifelse(sun_azimuth >= 0, sun_azimuth + 180, 180 + sun_azimuth) - 90

  if (verbose) {
    message(glue::glue("The calculation of sun position is done for ",
                       "{with_tz(date_time, tzone = 'UTC')}, in UTC timezone."))
  }

  # apply atmospheric refraction to sun elevation
  message(glue::glue("Atmospheric refraction for sun elevation {sun_elevation} is ",
                     "{.calculate_atmospheric_refraction(sun_elevation)}. ",
                     "Value is added to sun elevation."))

  sun_elevation <- sun_elevation + .calculate_atmospheric_refraction(sun_elevation)

  # output message
  if (verbose) {
    message(glue::glue("Coordinates of the raster center in latitude and longitude ",
                       "is {mid_point_long_lat[1,2]}, {mid_point_long_lat[1,1]}."))
    message(glue::glue("Sun azimuth is {sun_azimuth + 90}, and sun elevation is {sun_elevation}."))
  }

  # call general function
  .shade_lambert(surface_raster, sun_elevation, sun_azimuth, only_facing_sun, z_value)
}

#' @param sun_elevation Numeric value, from range 0 - 90, with 0 being sun elevation directly
#' on horizon while 90 means that sun is directly above the surface. Default value is \code{45}.
#' @param sun_azimuth Numeric value that specifies azimuth from which the sun is shining on the
#' surface. Value has to be from the range 0 - 360. Default value is \code{180}.
#'
#' @name shade_lambert
#'
#' @export
shade_lambert_sun_position <- function(surface_raster,
                                        sun_elevation = 45,
                                        sun_azimuth = 180,
                                        only_facing_sun = FALSE,
                                        z_value = 1,
                                        verbose = FALSE){

  .check_raster(surface_raster)

  .check_sun_elevation(sun_elevation)

  .check_sun_azimuth(sun_azimuth)

  sun_azimuth <- sun_azimuth - 90

  sun_elevation <- sun_elevation + .calculate_atmospheric_refraction(sun_elevation)

  # call general function
  .shade_lambert(surface_raster, sun_elevation, sun_azimuth, only_facing_sun, z_value)
}

#' @importFrom rayshader lamb_shade
#' @importFrom raster as.matrix mask values
.shade_lambert <- function(surface_raster,
                           sun_elevation = 45,
                           sun_azimuth = 180,
                           only_facing_sun = FALSE,
                           z_value = 1){

  matrix <- raster::as.matrix(surface_raster)

  matrix <- .flip_matrix_horizontally(matrix)

  shadow_matrix <- rayshader::lamb_shade(matrix,
                                         sunaltitude  = sun_elevation,
                                         sunangle = sun_azimuth,
                                         zscale = z_value,
                                         zero_negative = only_facing_sun)

  shade_raster <- surface_raster

  raster::values(shade_raster) <- shadow_matrix

  shade_raster <- raster::mask(shade_raster, surface_raster)

  shade_raster
}
