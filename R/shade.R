#' @title Function for calculation of rayshade on raster data
#'
#' @description This function is just a wrapper around \link[rayshader]{rayshade} that supports
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
#' @param sun_azimuth Numeric value that specifies azimuth from which the sun is shining on the
#' surface. Value has to be from the range 0 - 360. If not specified the sun azimuth is calculated
#' from date and sun elevation.
#' @param search_distance Distance in which sun blocking objects are searched for, specified in map
#' units. Default value is 200 map units.
#' @param light_intensity Boolean value indicating if Lambertian reflectance shoud be used to
#' determine surfaces facing directly to sun. Default value is \code{FALSE}. Package
#' \link[rayshader]{rayshade} names this parameter \code{lambert}.
#' @param verbose Should informative message be printed? Default \code{FALSE}.
#'
#' @return Object of class \linkS4class{Raster}.
#' @export
#'
#' @importFrom sp CRS SpatialPoints coordinates spTransform is.projected bbox
#' @importFrom raster bandnr crs values<- as.matrix
#' @importFrom suncalc getSunlightPosition getSunlightTimes
#' @importFrom rayshader rayshade
#' @importFrom lubridate ymd hms ymd_hms with_tz is.period
#'
shade <- function(surface_raster, date, time, tzone, sun_elevation, sun_azimuth, search_distance,
                  light_intensity, verbose) {
  UseMethod("shade", surface_raster)
}

#' @export
shade.RasterLayer <- function(surface_raster, date, time, tzone, sun_elevation, sun_azimuth, search_distance,
                              light_intensity = FALSE, verbose = FALSE) {

  use_given_sun_position <- !(missing(sun_azimuth)) & !(missing(sun_elevation))

  specified_date_time <- !missing(date) | !missing(time)

  # check if raster has CRS
  if (is.na(crs(surface_raster))) {
    stop("Raster does not have CRS. Please set up CRS for the data.",
         call. = FALSE)
  }

  # check if raster is projected to ensure that the calculation is done correctly
  if (!is.projected(crs(surface_raster))) {
    stop("Raster data must be projected. Otherwise can not properly determine shades.",
         call. = FALSE)
  }

  if (!(bandnr(surface_raster) == 1)) {
    stop(paste0("Raster must have only single layer containing the elevation.",
                "Currently it has, ", bandnr(surface_raster), " layers."),
         call. = FALSE)
  }

  # if no date specified use default
  if (missing(date)) {
    date <- "2018-03-20"
    if (!use_given_sun_position) {
      warning(paste0("Date is missing. Using ", date, " (spring equinox) as default."),
              call. = FALSE)
    }
  }

  # date string must be valid date
  if (is.na(ymd(date, quiet = TRUE)) & !use_given_sun_position) {
    stop(paste0("Provided date, \"", date,"\" can not be converted to date. Is it in format YYYY-MM-DD?"),
         call. = FALSE)
  }
  else{
    date <- ymd(date)
  }

  # if no time is specified use noon
  if (missing(time)) {
    time <- "noon"
    if (!use_given_sun_position) {
      warning(paste0("Time not set, using ", time, " as default."),
              call. = FALSE)
    }
  }

  # recode time string to valid variants for
  # getSunlightTimes function
  if (is.na(hms(time, quiet = TRUE)) & !use_given_sun_position) {
    # check if sun elevation string is a valid one
    if (time %in% c("noon", "sunrise", "sunset", "goldenHour", "goldenHourEnd")) {
      if (time == "noon") {
        time <- "solarNoon"
      }

      if (time == "sunrise") {
        time <- "sunriseEnd"
      }

      if (time == "sunset") {
        time <- "sunsetStart"
      }
    }
    else{
      stop(paste0("Unknown time character: ", time, ". \n",
                  "Known types are: noon, sunrise, sunset, goldenHour, goldenHourEnd."),
           call. = FALSE)
    }
  }
  else{
    time <- hms(time, quiet = TRUE)
  }

  # set default time zone if necessary
  if (missing(tzone) & !use_given_sun_position) {
    tzone = "UTC"
    warning("No time zone specified, using UTC as default.",
            call. = FALSE)
  }

  # if pnly one of sun_elevation or sun_azimuth is given, calculade shade by date and time
  # and output warning
  if (xor(missing(sun_elevation), missing(sun_azimuth))) {
    if (missing(sun_elevation)) {
      warning("Sun azimuth specified but sun elevation is missing. ",
              "Surface shading will be determined by date and time.",
              call. = FALSE)
    }

    if (missing(sun_azimuth)) {
      warning("Sun elevation specified but sun azimuth is missing. ",
              "Surface shading will be determined by date and time.",
              call. = FALSE)
    }
  }

  if (use_given_sun_position) {
    # sun elevation must be numeric from valid range 0 - 90
    if (!is.numeric(sun_elevation)) {
      stop(paste0("Sun elevation must be numeric. Currently it is: ",
                  class(sun_elevation), "."),
           call. = FALSE)
    }
    else if (is.numeric(sun_elevation) & (sun_elevation > 90 | sun_elevation < 0)) {
      stop(paste0("Sun elevation must be from range 0 - 90, otherwise the calculation does not make sense. ",
                  "Current value is: ", sun_elevation, "."),
           call. = FALSE)
    }

    # if sun azimuth is set then it must be from valid range 0 -360
    if (!is.numeric(sun_azimuth)) {
      stop(paste0("Sun azimuth must be numeric. Currently it is: ",
                  class(sun_azimuth), "."),
           call. = FALSE)
    }
    else if ((is.numeric(sun_azimuth) & (sun_azimuth > 360 | sun_azimuth < 0))) {
      stop(paste0("Sun azimuth must be from range 0 - 360, otherwise the calculation does not make sense. ",
                  "Current value is: ", sun_azimuth, "."),
           call. = FALSE)
    }
  }

  # if both date and sun azimuth and date are specified use azimuth
  if (use_given_sun_position & specified_date_time) {
    warning(paste0("Sun azimuth (", sun_azimuth, ") and sun elevation (", sun_elevation, ") are specified as
                   well as date (", date, ").",
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
  bbox <- bbox(surface_raster)
  x <- (bbox[1,2] + bbox[1,1]) / 2
  y <- (bbox[2,2] + bbox[2,1]) / 2
  xy <- cbind(x, y)

  mid_point <- SpatialPoints(xy, proj4string = CRS(projargs = crs(surface_raster)@projargs))

  mid_point_long_lat <- as.data.frame(coordinates(spTransform(mid_point, CRS("+proj=longlat +datum=WGS84"))))
  #-----------------------------------

  #-----------------------------------
  # calculate time for sun elevation
  if (!use_given_sun_position & is.character(time)) {
  time <- getSunlightTimes(date = date,
                           lat = mid_point_long_lat[,2], lon = mid_point_long_lat[,1],
                           keep = c(time))
  time <- time[,4]
  }
  #-----------------------------------

  #-----------------------------------
  # determine sun position
  if (!use_given_sun_position) {
    if (is.period(time)) {
      date_time <- ymd_hms(paste(date, time), tz = tzone)
    }
    else{
      date_time <- ymd_hms(time, tz = "UTC")
    }

    sun_pos <- getSunlightPosition(date = with_tz(date_time, tzone = "UTC"),
                                   lat = mid_point_long_lat[,2], lon = mid_point_long_lat[,1])

    sun_elevation <- (sun_pos$altitude * (180/pi))

    sun_azimuth <- sun_pos$azimuth * (180/pi)

    sun_azimuth <- ifelse(sun_azimuth >= 0, sun_azimuth + 180, 180 + sun_azimuth) - 90

    if (verbose) {
      message(paste0("The calculation of sun position is done for ",
                     with_tz(date_time, tzone = "UTC"),
                     " in UTC timezone."))
    }
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
  cell_size <- max((surface_raster@extent@ymax - surface_raster@extent@ymin) / surface_raster@nrows,
                   (surface_raster@extent@xmax - surface_raster@extent@xmin) / surface_raster@ncols)

  search_distance <- ceiling(search_distance / cell_size)
  #-----------------------------------

  matrix <- as.matrix(surface_raster)

  shadow_matrix <- rayshade(matrix,
                      anglebreaks = sun_elevation,
                      sunangle = sun_azimuth,
                      maxsearch = search_distance,
                      lambert = light_intensity,
                      multicore = TRUE,
                      remove_edges = FALSE)

  values(surface_raster) <- shadow_matrix

  return(surface_raster)
}


