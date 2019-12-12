#' @title Function for calculation of ambient shading on raster data
#'
#' @description This function is just a wrapper around \link[rayshader]{ambient_shade} that supports
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
#' @param rays_number Number of rays that are calculated around each pixel. Default value is 36,
#' which means that one ray per 10 degrees of orientation.
#' @param search_distance Distance in which sun blocking objects are searched for, specified in map
#' units. Default value is \code{200} map units.
#' @param z_value numeric value indicating the scale between xy and z. Typical use is if the distance
#' is measured in meters and elevation in foots, in such case the z value needs to be recalculated
#' to meters. Default value is \code{1}, which means that both measures use the same units.
#' @param verbose Should informative message be printed? Default \code{FALSE}.
#'
#' @return Object of class \linkS4class{Raster}.
#'
#' @export
#'
#' @name shade_ambient
#'
#' @importFrom suncalc getSunlightPosition
#' @importFrom rayshader ambient_shade
#' @importFrom lubridate ymd_hms with_tz
#'
shade_ambient_date_time <- function(surface_raster,
                                    date = as.character(Sys.Date()),
                                    time = "noon",
                                    tzone = "UTC",
                                    rays_number = 36,
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
  .shade_ambient(surface_raster, sun_elevation, rays_number, search_distance, z_value)
}

#' @param sun_elevation Numeric value, from range 0 - 90, with 0 being sun elevation directly
#' on horizon while 90 means that sun is directly above the surface.
#' @name shade_ambient
#'
#' @export
shade_ambient_sun_position <- function(surface_raster,
                                       sun_elevation = 45,
                                       rays_number = 36,
                                       search_distance = 200,
                                       z_value = 1,
                                       verbose = FALSE) {

  .check_raster(surface_raster)

  .check_sun_elevation(sun_elevation)

  sun_elevation <- sun_elevation + .calculate_atmospheric_refraction(sun_elevation)

  # call general function
  .shade_ambient(surface_raster, sun_elevation, rays_number, search_distance, z_value)
}

#' @importFrom rayshader ambient_shade
#' @importFrom raster as.matrix mask values
.shade_ambient <- function(surface_raster,
                           sun_elevation,
                           rays_number,
                           search_distance,
                           z_value){

  # determine search distance in pixels
  search_distance <- .get_search_distance(search_distance, surface_raster)

  matrix <- raster::as.matrix(surface_raster)

  matrix <- .flip_matrix_horizontally(matrix)

  shadow_matrix <- rayshader::ambient_shade(matrix,
                                            anglebreaks = sun_elevation,
                                            sunbreaks = rays_number,
                                            maxsearch = search_distance,
                                            multicore = TRUE,
                                            remove_edges = FALSE,
                                            zscale = z_value)

  shade_raster <- surface_raster

  raster::values(shade_raster) <- shadow_matrix

  shade_raster <- raster::mask(shade_raster, surface_raster)

  shade_raster
}
