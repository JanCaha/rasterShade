#' Extract sun position at given location in specific date and time
#'
#' @description The function is a wrapper around \link[suncalc]{getSunlightPosition}
#' function. It performs several validy checks on inputs to ensure that they are
#' logically valid. It also allows specification of time as and as an solar event.
#'
#' @param date Date specified as \code{character} (in form \code{YYYY-MM-DD}) or output of
#' \link[lubridate]{ymd} function.
#' @param time either character representation of time (in format \code{HH:MM:SS},
#' or output of \link[lubridate]{hms} function)
#' or one of text values: \code{"noon", "sunrise", "sunset", "goldenHour", "goldenHourEnd"}.
#' The textual representation is used to determine sun elevation using function
#' \link[suncalc]{getSunlightPosition}. Value \code{"sunrise"} represents end of sunrise,
#' while \code{"sunset"} represents start of sunset.
#' @param tzone a character string that specifies which time zone to parse the date with.
#' The string must be a time zone that is recognized by the user's OS. If no time zone is given the default
#' UTC is used.
#' @param lat latitude of the location. Numeric from range -90,90. Positive values are north
#' of the Equator.
#' @param lon longtitude of the location. Numeric from range -180,180. Positive values are
#' east of the central meridian.
#'
#' @return data.frame with one row and two columns sun_azimuth and sun_elevation
#' indicating sun position on the sky. Both values are in degrees. Sun azimuth value 0 is
#' pointing towards North and in rises clockwise direction. Sun elevation 0 means that sun
#' is exactly on the horizon, value 90 means that sun is exactly az zenith.
#'
#' @export
#'
#' @importFrom lubridate ymd hms ymd_hms with_tz is.period
#' @importFrom suncalc getSunlightPosition getSunlightTimes
#'
get_sun_position <- function(date, time, tzone = "UTC", lat, lon){

  # logical checks for latitude
  if (!is.numeric(lat)) {
    stop(paste0("Latitude must be numeric. It is: ", class(lat), "."))
  }else if (!(-90 <= lat & lat <= 90)) {
    stop(paste0("Latitude must be from range (-90,90), the current value (",
                lat, ") is not from this range."))
  }

  # logical checks for longtitude
  if (!is.numeric(lon)) {
    stop(paste0("Longitude must be numeric. It is: ", class(lon), "."))
  }else if (!(-180 <= lon & lon <= 180)) {
    stop(paste0("Longitude must be from range (-180,180), the current value (",
                lon, ") is not from this range."))
  }

  date <- .check_date(date)

  time <- .check_time(time)

  # if time is given as string calculate
  # real time of the event
  if (is.character(time)) {
    time <- .get_time_event(time, date, lat, lon)
  }

  date_time <- .check_time_tzone(time, date, tzone)

  # sun position
  sun_pos <- getSunlightPosition(date = with_tz(date_time, tzone = "UTC"),
                                 lat = lat, lon = lon)

  # recalculation into degrees and correct (north) orientation of azimuth
  sun_elevation <- sun_pos$altitude * (180/pi)

  sun_azimuth <- sun_pos$azimuth * (180/pi)

  sun_azimuth <- ifelse(sun_azimuth >= 0, sun_azimuth + 180, 180 + sun_azimuth)

  # table
  table <- data.frame(sun_azimuth = sun_azimuth, sun_elevation = sun_elevation)

  return(table)
}
