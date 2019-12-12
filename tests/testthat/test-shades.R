library(raster)

context("Checks shade_* functions")

r <- raster::raster(ncol = 10, nrow = 10)
raster::values(r) <- runif(raster::ncell(r))
raster::crs(r) <- "+proj=krovak +lat_0=49.5 +lon_0=24.83333333333333 +alpha=30.28813975277778 +k=0.9999
+x_0=0 +y_0=0 +ellps=bessel +units=m +no_defs"


test_that("shade_global", {

  expect_is(shade_global_date_time(r), "RasterLayer")
  expect_is(shade_global_sun_position(r), "RasterLayer")
})

test_that("shade_lambert", {

  expect_is(shade_lambert_date_time(r), "RasterLayer")
  expect_is(shade_lambert_sun_position(r), "RasterLayer")
})

test_that("shade_ambient", {

  expect_is(shade_ambient_date_time(r), "RasterLayer")
  expect_is(shade_ambient_sun_position(r), "RasterLayer")
})
