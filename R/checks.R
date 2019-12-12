# general functions that performs checks on various input data types
# functions here are used in more than one function


# checks if the raster is valid for operations
#' @importFrom raster crs isLonLat bandnr
#' @importFrom glue glue
#' @importFrom assertthat assert_that
#' @importFrom methods is
.check_raster <- function(raster) {

  assertthat::assert_that(is(raster, "RasterLayer"),
                          msg = glue::glue("Raster have to be of class `RasterLayer`. ",
                                           "It is: `{glue::glue_collapse(class(raster), sep = ', ')}`."))

  # check if raster has CRS
  assertthat::assert_that(!is.na(raster::crs(raster)),
                          msg = "Raster does not have CRS. Please set up CRS for the data.")

  # check if raster is projected to ensure that the calculation is done correctly
  assertthat::assert_that(!raster::isLonLat(raster),
                          msg = "Raster data must be projected. Otherwise can not properly determine shades.")

  # not needed most likely
  # raster can have only one band
  # assertthat::assert_that(raster::bandnr(raster) == 1,
  #                         msg = glue::glue("Raster must have only single band containing the elevation.",
  #                                          "Currently it has, `{bandnr(raster)}` bands."))
}

# checks if the string is a date and converts it to Date
#' @importFrom lubridate ymd
#' @importFrom assertthat assert_that is.string
.check_return_date <- function(date){

  assertthat::assert_that(assertthat::is.string(date),
                          msg = glue::glue("Date has to be string. It is: `{class(date)}`."))

  assertthat::assert_that(!is.na(lubridate::ymd(date, quiet = TRUE)),
                          msg = glue::glue("Provided date, `{date}` can not be converted to date.",
                                           " Is it in format YYYY-MM-DD?"))
  lubridate::ymd(date)
}

# #' @importFrom lubridate is.period ymd_hms
# .check_time_tzone <- function(time, date, tzone) {
#   warning("using unfixed function")
#   # if time is specified as time then use it with timezone
#   if (lubridate::is.period(time)) {
#    date_time <- lubridate::ymd_hms(paste(date, time), tz = tzone)
#   }
#   else{
#     date_time <- lubridate::ymd_hms(time, tz = "UTC")
#   }
#
#   date_time
# }

# #' check if time is one of the available words or if it can be converted to time
# #' does the conversion
# .check_time <- function(time){
#   # recode time string to valid variants for
#   # getSunlightTimes function
#   if (is.na(hms(time, quiet = TRUE))) {
#     # check if sun elevation string is a valid one
#     if (time %in% c("noon", "sunrise", "sunset", "goldenHour", "goldenHourEnd")) {
#       if (time == "noon") {
#         time <- "solarNoon"
#       }
#
#       if (time == "sunrise") {
#         time <- "sunriseEnd"
#       }
#
#       if (time == "sunset") {
#         time <- "sunsetStart"
#       }
#     }
#     else{
#       stop(paste0("Unknown time character: ", time, ". \n",
#                   "Known types are: noon, sunrise, sunset, goldenHour, goldenHourEnd."),
#            call. = FALSE)
#     }
#   }
#   else{
#     if (is.na(hms(time, quiet = TRUE))) {
#       stop(paste0("Provided time, \"", time,"\" can not be converted to time.",
#                   " Is it in format HH-MM-SS?"),
#            call. = FALSE)
#     }else{
#       time <- hms(time, quiet = TRUE)
#     }
#   }
#
#   return(time)
# }

# sun elevation must be numeric from valid range 0 - 90
#' @importFrom glue glue
#' @importFrom assertthat assert_that
.check_sun_elevation <- function(sun_elevation) {

  assertthat::assert_that(is.numeric(sun_elevation),
                          msg = glue::glue("Sun elevation must be numeric. ",
                                           "Currently it is: `{class(sun_elevation)}`."))

  assertthat::assert_that(0 <= sun_elevation & sun_elevation <= 90,
                          msg = glue::glue("Sun elevation must be from range 0 - 90, ",
                                           "otherwise the calculation does not make sense. ",
                                           "Current value is: `{sun_elevation}`."))
}

# if sun azimuth is set then it must be from valid range 0 -360
#' @importFrom glue glue
#' @importFrom assertthat assert_that
.check_sun_azimuth <- function(sun_azimuth) {

  assertthat::assert_that(is.numeric(sun_azimuth),
                          msg = glue::glue("Sun azimuth must be numeric. ",
                                           "Currently it is: `{class(sun_azimuth)}`."))

  assertthat::assert_that(0 <= sun_azimuth & sun_azimuth <= 360,
                          msg = glue::glue("Sun azimuth must be from range 0 - 360, ",
                                           "otherwise the calculation does not make sense. ",
                                           "Current value is: `{sun_azimuth}`."))
}

# check if timezone is correct
#' @importFrom assertthat assert_that is.string
.check_TZ <- function(timezone){

  assertthat::assert_that(assertthat::is.string(timezone),
                          msg = "Timezone is not a string.")

  assertthat::assert_that(timezone %in% OlsonNames(tzdir = NULL),
                          msg = "Timezone not found in `OlsonNames`.")
}

# check if Latitude is correct
#' @importFrom assertthat assert_that is.string
#' @importFrom glue glue
.check_lat <- function(lat){

  assertthat::assert_that(assertthat::is.number(lat),
                          msg = glue::glue("Latitude must be numeric. It is: `{class(lat)}`."))

  assertthat::assert_that(-90 <= lat & lat <= 90,
                          msg = glue::glue("Latitude must be from range (-90, 90). ",
                          "The current value `{lat}` is not from this range."))

}

# check if Longitude is correct
#' @importFrom assertthat assert_that is.string
#' @importFrom glue glue
.check_lon <- function(lon){

  assertthat::assert_that(assertthat::is.number(lon),
                          msg = glue::glue("Longitude must be numeric. It is: `{class(lon)}`."))

  assertthat::assert_that(-180 <= lon & lon <= 180,
                          msg = glue::glue("Longitude must be from range (-180, 180). ",
                                           "The current value `{lon}` is not from this range."))

}

#' @importFrom hms as_hms
.check_return_time <- function(time){

  if (!hms::is_hms(time)) {

    assertthat::assert_that(assertthat::is.string(time),
                           msg = glue::glue("Time has to be string. It is: `{class(time)}`."))

    hms_time <- suppressWarnings(hms::as_hms(time))

    assertthat::assert_that(!is.na(hms_time),
                            msg = glue::glue("Time does not have the right format. ",
                                             "Is it HH:MM:SS? Time is: `{time}`."))

    return(hms_time)
  }

  time
}

