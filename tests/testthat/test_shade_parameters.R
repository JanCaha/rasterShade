test_that("Test shade parameters: ", {

  r <- raster(ncol = 10, nrow = 10)
  values(r) <- runif(ncell(r))
  crs(r) <- "+proj=krovak +lat_0=49.5 +lon_0=24.83333333333333 +alpha=30.28813975277778 +k=0.9999
  +x_0=0 +y_0=0 +ellps=bessel +units=m +no_defs"

  expect_warning(shade(r), "Date is missing.")
  expect_error(shade(r, date = "asdfg"), "can not be converted to date")

  expect_warning(shade(r), "Time not set")
  expect_error(shade(r, time = "dusk"), "Unknown time character")

  expect_warning(shade(r), "No time zone specified")


  expect_warning(shade(r, sun_elevation = 12), "Sun elevation specified")
  expect_warning(shade(r, sun_azimuth = 120), "Sun azimuth specified")

  expect_warning(shade(r), "Search distance not set")

  expect_error(shade(r, sun_azimuth = 45, sun_elevation = -1), "Sun elevation must be from range 0 - 90")
  expect_error(shade(r, sun_azimuth = 45, sun_elevation = 91), "Sun elevation must be from range 0 - 90")
  expect_error(shade(r, sun_azimuth = 45, sun_elevation = "asdf"), "Sun elevation must be numeric")

  expect_error(shade(r, sun_azimuth = -1, sun_elevation = 35), "Sun azimuth must be from range 0 - 360")
  expect_error(shade(r, sun_azimuth = 361, sun_elevation = 35), "Sun azimuth must be from range 0 - 360")
  expect_error(shade(r, sun_azimuth = "asdf", sun_elevation = 35), "Sun azimuth must be numeric")

  expect_is(shade(r, sun_azimuth = 180, sun_elevation = 45), "RasterLayer")

  expect_warning(shade(r, date = "2018-03-21" , sun_azimuth = 165, sun_elevation = 45),
               "Sun azimuth and elevation are considered more important and will be used")

  expect_error(shade(r, time = "19:00:00", tzone = "America/Vancouver"),
               "Sun elevation above horizon is negative")
})
