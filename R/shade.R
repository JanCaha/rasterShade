#' @title Function for calculation of rayshade on raster data
#'
#' @description This function is just a wrapper around \link[rayshader]{rayshade} that supports
#' directly using \linkS4class{Raster} in the function. The function also tries to set
#' "reasonable" default values for parameters.
#'
#' @param raster Object of class \linkS4class{Raster}.
#' @param date Date specified as \code{character} in form \code{YYYY-MM-DD}. Default value is
#' spring equinox (2018-03-20), when the sun is in the mean position.
#' @param sun_elevation Either numeric value, from range 0 - 90, with 0 being sun elevation directly
#' on horizon while 90 means that sun is directly above the surface, or one of text values:
#' \code{"noon", "sunrise", "sunset", "goldenHour", "goldenHourEnd"}. The textual representation is
#' used to determine sun elevation using function \link[suncalc]{getSunlightPosition}. Value
#' \code{"sunrise"} represents end of sunrise, while \code{"sunset"} represents start of sunset.
#' Default value is \code{"noon"}.
#' @param sun_azimuth Numeric value that specifies azimuth from which the sun is shining on the
#' surface. Value has to be from the range 0 - 360. If not specified the sun azimuth is calculated
#' from date and sun elevation.
#' @param search_distance Distance in which sun blocking objects are searched for, specified in map
#' units. Default value is 200 map units.
#'
#' @return Object of class \linkS4class{Raster}.
#' @export
#'
#' @importFrom sp CRS SpatialPoints coordinates spTransform is.projected
#' @importFrom raster bandnr crs values<-
#' @importFrom suncalc getSunlightPosition getSunlightTimes
#' @importFrom rayshader rayshade
#'
shade <- function(raster, date, sun_elevation, sun_azimuth, search_distance) {
  UseMethod("shade", raster)
}

#' @export
shade.RasterLayer <- function(raster, date, sun_elevation, sun_azimuth, search_distance) {

  # check if raster is projected to ensure that the calculation is done correctly
  if (!is.projected(crs(raster))) {
    stop("Raster data must be projected. Otherwise can not properly determine shades.",
         call. = FALSE)
  }

  if (!(bandnr(raster) == 1)) {
    stop(paste0("Raster must have only single layer containing the elevation.",
                "Currently it has, ", bandnr(raster), " layers."),
         call. = FALSE)
  }

  # if no date specified use default
  if (missing(date)) {
    date <- "2018-03-20"
    warning(paste0("Date is missing. Using ", date, " (spring equinox) as default."),
            call. = FALSE)
  }

  # date string must be valid date
  if (!is.convertible.to.date(date)) {
    stop(paste0("Provided date, \"", date,"\" can not be converted to date. Is it YYYY-MM-DD?"),
         call. = FALSE)
  }

  # if no sun elevation is specified use noon
  if (missing(sun_elevation)) {
    sun_elevation <- "noon"
    warning(paste0("Sun elevation not set, using ", sun_elevation, " as default."),
            call. = FALSE)
  }

  # check if sun elevation string is a valid one
  if (is.character(sun_elevation) & !(sun_elevation %in%
                                     c("noon", "sunrise", "sunset", "goldenHour", "goldenHourEnd"))) {
    stop(paste0("Unknown sun_evelation character: ", sun_elevation, ". \n",
                "Known types are: noon, sunrise, sunset, goldenHour, goldenHourEnd."),
         call. = FALSE)
  }

  # recode sun elevation string to valid variants for
  # getSunlightTimes function
  if (is.character(sun_elevation) & (sun_elevation %in%
                                    c("noon", "sunrise", "sunset", "goldenHour", "goldenHourEnd"))) {
    if (sun_elevation == "noon"){
      sun_elevation = "solarNoon"
    }

    if (sun_elevation == "sunrise"){
      sun_elevation == "sunriseEnd"
    }

    if (sun_elevation == "sunset"){
      sun_elevation == "sunsetStart"
    }
  }

  # sun elevation must be numeric from valid range 0 - 90
  if (is.numeric(sun_elevation) & (sun_elevation > 90 | sun_elevation < 0)) {
    stop(paste0("Sun elevation must be from range 0 - 90, otherwise the calculation does not make sense. ",
                "Current value is: ", sun_elevation, "."),
         call. = FALSE)
  }

  # if sun azimuth is set then it must be from valid range 0 -360
  if (!missing(sun_azimuth)) {
    if ((is.numeric(sun_azimuth) & (sun_azimuth > 360 | sun_azimuth < 0))) {
      stop(paste0("Sun azimuth must be from range 0 - 360, otherwise the calculation does not make sense. ",
                  "Current value is: ", sun_azimuth, "."),
           call. = FALSE)
    }
  }

  # if both date and sun azimuth and date are specified use azimuth
  if (!missing(sun_azimuth) & !missing(date)) {
    warning(paste0("Both sun azimuth (", sun_azimuth, ") and date (", date, ") are set.",
                   " Sun azimuth is considered more important and will be used."),
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
  bbox <- bbox(raster)
  x <- bbox[1,2] - bbox[1,1]
  y <- bbox[2,2] - bbox[2,1]
  xy <- cbind(x, y)

  mid_point <- SpatialPoints(xy, proj4string = CRS(projargs = crs(raster)@projargs))

  mid_point_long_lat <- as.data.frame(coordinates(spTransform(mid_point, CRS("+proj=longlat +datum=WGS84"))))
  #-----------------------------------

  #-----------------------------------
  # calculate time for sun elevation
  if (!missing(date)){
  time <- getSunlightTimes(date = as.Date(date),
                           lat = mid_point_long_lat[,2], lon = mid_point_long_lat[,1],
                           keep = c(sun_elevation),
                           tz = "UTC")
  time <- time[,4]
  }
  #-----------------------------------

  #-----------------------------------
  # determine sun position
  sun_pos <- getSunlightPosition(date = time,
                                 lat = mid_point_long_lat[,2], lon = mid_point_long_lat[,1])

  if (!is.numeric(sun_elevation)) {
    sun_elevation <- (sun_pos$altitude * (180/pi))
  }

  if (missing(sun_azimuth)) {
    sun_azimuth <- sun_pos$azimuth * (180/pi)

    sun_azimuth <- ifelse(sun_azimuth >= 0, sun_azimuth + 180, 180 + sun_azimuth) - 90
  }
  else if (!is.numeric(sun_azimuth)) {
    sun_azimuth <- sun_pos$azimuth * (180/pi)

    sun_azimuth <- ifelse(sun_azimuth >= 0, sun_azimuth + 180, 180 + sun_azimuth) - 90
  }
  #-----------------------------------

  #-----------------------------------
  # determine sun position
  cell_size <- max((raster@extent@ymax - raster@extent@ymin) / raster@nrows,
                   (raster@extent@xmax - raster@extent@xmin) / raster@ncols)

  search_distance <- ceiling(search_distance / cell_size)
  #-----------------------------------

  matrix <- as.matrix(raster)

  shadow_matrix <- rayshade(matrix,
                      anglebreaks = sun_elevation,
                      sunangle = sun_azimuth,
                      maxsearch = search_distance,
                      lambert = FALSE,
                      multicore = TRUE,
                      remove_edges = FALSE)

  values(raster) <- shadow_matrix

  return(raster)
}


