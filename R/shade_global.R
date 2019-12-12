#' @title Function for calculation of global shading on raster data
#'
#' @description This function is just a wrapper around \link[rayshader]{ray_shade} that supports
#' directly using \linkS4class{Raster} in the function. The function also tries to set
#' "reasonable" default values for parameters.
#'
#' @param surface_raster Object of class \linkS4class{RasterLayer}.
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
#' @param search_distance Distance in which sun blocking objects are searched for, specified in map
#' units. Default value is \code{200} map units.
#' @param z_value numeric value indicating the scale between xy and z. Typical use is if the distance
#' is measured in meters and elevation in foots, in such case the z value needs to be recalculated
#' to meters. Default value is \code{1}, which means that both measures use the same units.
#' @param verbose Should informative message be printed? Default \code{FALSE}.
#'
#' @name shade_global
#'
#' @return Object of class \linkS4class{Raster}.
#' @export
#'
#' @importFrom suncalc getSunlightPosition
#' @importFrom lubridate ymd_hms with_tz
#'
shade_global_date_time <- function(surface_raster,
                                   date = as.character(Sys.Date()),
                                   time = "noon",
                                   tzone = "UTC",
                                   search_distance = 200,
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

  sun_pos <- getSunlightPosition(date = lubridate::with_tz(date_time, tzone = "UTC"),
                                 lat = mid_point_long_lat[,2], lon = mid_point_long_lat[,1])

  sun_elevation <- sun_pos$altitude * (180/pi)

  sun_azimuth <- sun_pos$azimuth * (180/pi)

  sun_azimuth <- ifelse(sun_azimuth >= 0, sun_azimuth + 180, 180 + sun_azimuth) - 90

  if (verbose) {
    message(glue::glue("The calculation of sun position is done for ",
                       "{with_tz(date_time, tzone = 'UTC')}, in UTC timezone."))
  }

  # apply atmospheric refraction to sun elevation
  if (verbose) {
    message(glue::glue("Atmospheric refraction for sun elevation {sun_elevation} is ",
                       "{.calculate_atmospheric_refraction(sun_elevation)}. ",
                       "Value is added to sun elevation."))
  }

  sun_elevation <- sun_elevation + .calculate_atmospheric_refraction(sun_elevation)

  # output message
  if (verbose) {
    message(glue::glue("Coordinates of the raster center in latitude and longitude ",
                       "is {mid_point_long_lat[1,2]}, {mid_point_long_lat[1,1]}."))
    message(glue::glue("Sun azimuth is {sun_azimuth + 90}, and sun elevation is {sun_elevation}."))
  }

  # call general function
  .shade_global(surface_raster, sun_elevation, sun_azimuth, search_distance, z_value)
}

#' @param sun_elevation Numeric value, from range 0 - 90, with 0 being sun elevation directly
#' on horizon while 90 means that sun is directly above the surface. Default value is \code{45}.
#' @param sun_azimuth Numeric value that specifies azimuth from which the sun is shining on the
#' surface. Value has to be from the range 0 - 360. Default value is \code{180}.
#'
#' @name shade_global
#'
#' @export
shade_global_sun_position <- function(surface_raster,
                                      sun_elevation = 45,
                                      sun_azimuth = 180,
                                      search_distance = 200,
                                      z_value = 1,
                                      verbose = FALSE) {
  .check_raster(surface_raster)

  .check_sun_elevation(sun_elevation)

  .check_sun_azimuth(sun_azimuth)

  sun_azimuth <- sun_azimuth - 90

  sun_elevation <- sun_elevation + .calculate_atmospheric_refraction(sun_elevation)

  # call general function
  .shade_global(surface_raster, sun_elevation, sun_azimuth, search_distance, z_value)
}

#' @importFrom rayshader ray_shade
#' @importFrom raster as.matrix mask values
.shade_global <- function(surface_raster, sun_elevation, sun_azimuth,
                          search_distance, z_value){

  # determine search distance in pixels
  search_distance <- .get_search_distance(search_distance, surface_raster)

  matrix <- raster::as.matrix(surface_raster)

  matrix <- .flip_matrix_horizontally(matrix)

  shadow_matrix <- rayshader::ray_shade(matrix,
                                        anglebreaks = sun_elevation,
                                        sunangle = sun_azimuth,
                                        maxsearch = search_distance,
                                        lambert = FALSE,
                                        multicore = TRUE,
                                        zscale = z_value)

  shade_raster <- surface_raster

  raster::values(shade_raster) <- shadow_matrix

  shade_raster <- raster::mask(shade_raster, surface_raster)

  shade_raster
}
