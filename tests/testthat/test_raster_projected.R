context("Checks if raster is valid for operation")

test_that("Test raster projected: ", {

  r <- raster::raster(ncol = 10, nrow = 10)
  raster::values(r) <- runif(raster::ncell(r))

  expect_error(shade(r), "must be projected")

  raster::crs(r) <- NULL

  expect_error(shade(r), "Raster does not have CRS")

  raster::crs(r) <- "+proj=krovak +lat_0=49.5 +lon_0=24.83333333333333 +alpha=30.28813975277778 +k=0.9999
  +x_0=0 +y_0=0 +ellps=bessel +units=m +no_defs"

  expect_is(shade(r), "RasterLayer")
})
