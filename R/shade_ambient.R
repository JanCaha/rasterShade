#' @title Function for calculation of ambient shading on raster data
#'
#' @description This function is just a wrapper around \link[rayshader]{ambient_shade} that supports
#' directly using \linkS4class{Raster} in the function. The function also tries to set
#' "reasonable" default values for parameters.
#'
#' @param surface_raster Object of class \linkS4class{Raster}.
#' @param date Date specified as \code{character} in form \code{YYYY-MM-DD}. Default value is
#' spring equinox (2018-03-20), when the sun is in the mean position.
#' @param time Either character representation of time (in format \code{HH:MM:SS}) or one of text values:
#' \code{"noon", "sunrise", "sunset", "goldenHour", "goldenHourEnd"}. The textual representation is
#' used to determine sun elevation using function \link[suncalc]{getSunlightPosition}. Value
#' \code{"sunrise"} represents end of sunrise, while \code{"sunset"} represents start of sunset.
#' Default value is \code{"noon"}.
#' @param tzone a character string that specifies which time zone to parse the date with.
#' The string must be a time zone that is recognized by the user's OS. If no time zone is given the default
#' UTC is used.
#' @param sun_elevation Numeric value, from range 0 - 90, with 0 being sun elevation directly
#' on horizon while 90 means that sun is directly above the surface.
#' @param rays_number Number of rays that are calculated around each pixel. Default value is 36,
#' which means that one ray per 10 degrees of orientation.
#' @param search_distance Distance in which sun blocking objects are searched for, specified in map
#' units. Default value is 200 map units.
#' @param z_value numeric value indicating the scale between xy and z. Typical use is if the distance
#' is measured in meters and elevation in foots, in such case the z value needs to be recalculated
#' to meters. Default value is 1, which means that both measures use the same units.
#' @param verbose Should informative message be printed? Default \code{FALSE}.
#'
#' @return Object of class \linkS4class{Raster}.
#' @export
#'
#' @importFrom sp CRS SpatialPoints coordinates spTransform is.projected bbox
#' @importFrom raster bandnr crs values<- as.matrix extract extent
#' @importFrom suncalc getSunlightPosition getSunlightTimes
#' @importFrom rayshader ambient_shade
#' @importFrom lubridate ymd hms ymd_hms with_tz is.period
#'
shade_ambient <- function(surface_raster, date, time, tzone, sun_elevation, rays_number,
                         search_distance, z_value, verbose) {
  UseMethod("shade_ambient", surface_raster)
}

#' @export
shade_ambient.RasterLayer <- function(surface_raster, date, time, tzone, sun_elevation,
                                      rays_number = 36, search_distance, z_value = 1,
                                      verbose = FALSE) {

  use_given_sun_position <- !(missing(sun_elevation))

  specified_date_time <- !missing(date) | !missing(time)

  .check_raster(surface_raster)

  # if no date specified use default
  if (missing(date)) {
    date <- "2018-03-20"
    if (!use_given_sun_position) {
      warning(paste0("Date is missing. Using ", date, " (spring equinox) as default."),
              call. = FALSE)
    }
  }

  date <- .check_date(date)

  # if no time is specified use noon
  if (missing(time)) {
    time <- "noon"
    if (!use_given_sun_position) {
      warning(paste0("Time not set, using ", time, " as default."),
              call. = FALSE)
    }
  }

  if (!use_given_sun_position) {
    time <- .check_time(time)
  }

  # set default time zone if necessary
  if (missing(tzone) & !use_given_sun_position) {
    tzone = "UTC"
    warning("No time zone specified, using UTC as default.",
            call. = FALSE)
  }

  if (use_given_sun_position) {

    .check_sun_elevation(sun_elevation)
  }

  # if both date and sun azimuth and date are specified use azimuth
  if (use_given_sun_position & specified_date_time) {
    warning(paste0("Sun elevation (", sun_elevation, ") is specified as well as date (", date, ").",
                   " Sun azimuth and elevation are considered more important and will be used."),
            call. = FALSE)
  }

  #  search distance not specified use default value 200
  if (missing(search_distance)) {
    search_distance <- 200
    warning(paste0("Search distance not set. Using default value - ", search_distance, " map units. ",
                   "Consider if the value is suitable for the purpose."),
            call. = FALSE)
  }

  #-----------------------------------
  # determine midpoint of the raster as long lat coordinates for further calculations of sun position
  mid_point_long_lat <- .get_midpoint_coordinates(surface_raster)

  #-----------------------------------

  #-----------------------------------
  # calculate time for sun elevation
  if (!use_given_sun_position & is.character(time)) {
    time <- .get_time_event(time, date, mid_point_long_lat[,2], mid_point_long_lat[,1])
  }
  #-----------------------------------

  #-----------------------------------
  # determine sun position
  if (!use_given_sun_position) {

    date_time <- .check_time_tzone(time, date, tzone)

    sun_pos <- getSunlightPosition(date = with_tz(date_time, tzone = "UTC"),
                                   lat = mid_point_long_lat[,2], lon = mid_point_long_lat[,1])

    sun_elevation <- sun_pos$altitude * (180/pi)

    sun_azimuth <- sun_pos$azimuth * (180/pi)

    sun_azimuth <- ifelse(sun_azimuth >= 0, sun_azimuth + 180, 180 + sun_azimuth) - 90

    if (verbose) {
      message(paste0("The calculation of sun position is done for ",
                     with_tz(date_time, tzone = "UTC"),
                     " in UTC timezone."))
    }
  }else{
    sun_azimuth <- sun_azimuth - 90
  }
  #-----------------------------------

  #-----------------------------------
  # apply atmospheric refraction to sun elevation
  if (verbose) {
    message(paste0("Atmospheric refraction for sun elevation ", sun_elevation, " is ",
                   .calculate_atmospheric_refraction(sun_elevation), ". ",
                   "Value is added to sun elevation."))
  }

  sun_elevation <- sun_elevation + .calculate_atmospheric_refraction(sun_elevation)
  #-----------------------------------

  #-----------------------------------
  # output message
  if (verbose) {
    message(paste0("Coordinates of the raster center in latitude and longitude is ",
                   mid_point_long_lat[1,2], ", ", mid_point_long_lat[1,1], "."))
    message(paste0("Sun azimuth is ", sun_azimuth + 90, " and sun elevation is ", sun_elevation, "."))
  }

  # if the sun elevation is lower than zero even afer the applying the refraction,
  # the calculation does not make sense
  if (sun_elevation < 0) {
    stop(paste0("Sun elevation above horizon is negative (", sun_elevation, " degrees). ",
                "It is not possible to calculate shades."),
         call. = FALSE)
  }
  #-----------------------------------

  #-----------------------------------
  # determine search distance in pixels
  search_distance <- .get_search_distance(search_distance, surface_raster)
  #-----------------------------------

  matrix <- as.matrix(surface_raster)

  matrix <- .flip_matrix_horizontally(matrix)

  shadow_matrix <- ambient_shade(matrix,
                             anglebreaks = sun_elevation,
                             sunbreaks = rays_number,
                             maxsearch = search_distance,
                             multicore = TRUE,
                             remove_edges = FALSE,
                             zscale = z_value)

  values(surface_raster) <- shadow_matrix

  return(surface_raster)
}
