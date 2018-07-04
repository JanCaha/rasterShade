# general functions that performs checks on various input data types
# functions here are used in more than one function



# checks if the raster is valid for operations
.check_raster <- function(raster) {

  # check if raster has CRS
  if (is.na(crs(raster))) {
    stop("Raster does not have CRS. Please set up CRS for the data.",
         call. = FALSE)
  }

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
}

# checks if the string is a date and converts it to Date
.check_date <- function(date){
  # date string must be valid date
  if (is.na(ymd(date, quiet = TRUE))) {
    stop(paste0("Provided date, \"", date,"\" can not be converted to date.",
                " Is it in format YYYY-MM-DD?"),
         call. = FALSE)
  }else{
    date <- ymd(date)
  }

  return(date)
}


.check_time_tzone <- function(time, date, tzone) {
  # if time is specified as time then use it with timezone
  if (is.period(time)) {
    date_time <- ymd_hms(paste(date, time), tz = tzone)
  }
  else{
    date_time <- ymd_hms(time, tz = "UTC")
  }

  return(date_time)
}

# check if time is one of the available words or if it can be converted to time
# does the conversion
.check_time <- function(time){
  # recode time string to valid variants for
  # getSunlightTimes function
  if (is.na(hms(time, quiet = TRUE))) {
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
    if (is.na(hms(time, quiet = TRUE))) {
      stop(paste0("Provided time, \"", time,"\" can not be converted to time.",
                  " Is it in format HH-MM-SS?"),
           call. = FALSE)
    }else{
      time <- hms(time, quiet = TRUE)
    }
  }

  return(time)
}

.check_sun_elevation <- function(sun_elevation) {
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
}

.check_sun_azimuth <- function(sun_azimuth) {
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
