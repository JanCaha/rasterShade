context("Checks util and checking function")

test_that(".check_raster function", {

  expect_error(.check_raster(5),
               regexp = "Raster have to be of class `RasterLayer`. It is: `numeric`.")

  r <- raster::raster(ncol = 10, nrow = 10)
  raster::values(r) <- runif(raster::ncell(r))
  raster::crs(r) <- NULL

  expect_error(.check_raster(r),
               regexp = "Raster does not have CRS. Please set up CRS for the data.")

  r <- raster::raster(ncol = 10, nrow = 10)
  raster::values(r) <- runif(raster::ncell(r))

  expect_error(.check_raster(r),
               regexp = "Raster data must be projected. Otherwise can not properly determine shades.")
})

test_that(".check_sun_azimuth function", {

  expect_error(.check_sun_azimuth("a"),
               regexp = "Sun azimuth must be numeric. Currently it is: `character`.")
  expect_error(.check_sun_azimuth(-1),
               regexp = "Sun azimuth must be from range 0 - 360, otherwise the calculation does not make sense. Current value is: `-1`.")
  expect_error(.check_sun_azimuth(361),
               regexp = "Sun azimuth must be from range 0 - 360, otherwise the calculation does not make sense. Current value is: `361`.")


  expect_silent(.check_sun_azimuth(0))
  expect_silent(.check_sun_azimuth(180))
  expect_silent(.check_sun_azimuth(360))
})


test_that(".check_sun_elevation function", {

  expect_error(.check_sun_elevation("a"),
               regexp = "Sun elevation must be numeric. Currently it is: `character`.")
  expect_error(.check_sun_elevation(-1),
               regexp = "Sun elevation must be from range 0 - 90, otherwise the calculation does not make sense. Current value is: `-1`.")
  expect_error(.check_sun_elevation(91),
               regexp = "Sun elevation must be from range 0 - 90, otherwise the calculation does not make sense. Current value is: `91`.")


  expect_silent(.check_sun_elevation(0))
  expect_silent(.check_sun_elevation(45))
  expect_silent(.check_sun_elevation(90))
})

test_that(".check_TZ function", {

  expect_error(.check_TZ(5),
               regexp = "Timezone is not a string.")

  expect_error(.check_TZ("Random timezone"),
               regexp = "Timezone not found in `OlsonNames`.")

  expect_silent(.check_TZ("UTC"))
  expect_silent(.check_TZ("Europe/Prague"))
  expect_silent(.check_TZ("US/Pacific"))
})

test_that(".check_return_date function", {

  expect_error(.check_return_date(5),
               regexp = "Date has to be string. It is: `numeric`.")

  expect_error(.check_return_date("date"),
               regexp = "Provided date, `date` can not be converted to date.")

  expect_error(.check_return_date("2019-15-45"),
               regexp = "Provided date, `2019-15-45` can not be converted to date.")

  date <- "2019-05-12"
  expect_equal(as.character(.check_return_date(date)), date)
})

test_that(".check_return_time function", {

  expect_error(.check_return_time(5),
               regexp = "Time has to be string. It is: `numeric`.")

  expect_error(.check_return_time("Random time"),
               regexp = "Time does not have the right format.")

  expect_error(.check_return_time("25:68:92"),
               regexp = "Time is: `25:68:92`")

  time <- "15:34:08"
  expect_equal(as.character(.check_return_time(time)), time)

  time <- "01:05:01"
  expect_equal(as.character(.check_return_time("01:05:01")), time)

  time <- "1:5:1"
  expect_equal(as.character(.check_return_time(time)), "01:05:01")

  time <- hms::as_hms("01:05:01")
  expect_equal(.check_return_time(time), time)
})


test_that(".check_lon .check_lat functions", {

  expect_error(.check_lat("test"),
               regex = "Latitude must be numeric. It is: `character`.")
  expect_error(.check_lon("test"),
               regex = "Longitude must be numeric. It is: `character`.")

  expect_error(.check_lat(95),
               regex = "Latitude must be from range \\(-90, 90\\).")
  expect_error(.check_lon(181),
               regex = "Longitude must be from range \\(-180, 180\\).")

  expect_silent(.check_lat(0))
  expect_silent(.check_lon(0))
})
