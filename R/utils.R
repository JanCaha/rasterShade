# simple functions

# extra time (HH:MM:SS) of event based on textual specification of event - e.g. "noon",
# for give date at location (lat, lon)
#' @importFrom suncalc getSunlightTimes
.get_time_event <- function(time, date, lat, lon) {

  time <- suncalc::getSunlightTimes(date = date,
                                    lat = lat, lon = lon,
                                    keep = c(time))

  strsplit(as.character(time[,4]), " ")[[1]][2]
}

# returns midpoint of raster as data.frame (lat, lon)
#' @importFrom sp bbox SpatialPoints CRS spTransform coordinates
.get_midpoint_coordinates <- function(raster) {

  bbox <- sp::bbox(raster)
  x <- (bbox[1,2] + bbox[1,1]) / 2
  y <- (bbox[2,2] + bbox[2,1]) / 2
  xy <- cbind(x, y)

  mid_point <- sp::SpatialPoints(xy, proj4string = CRS(projargs = crs(raster)@projargs))

  mid_point_long_lat <- as.data.frame(sp::coordinates(sp::spTransform(mid_point, sp::CRS("+proj=longlat +datum=WGS84"))))

  mid_point_long_lat
}

# determine search distance in pixels
.get_search_distance <- function(search_distance, raster){

  cell_size <- max((raster@extent@ymax - raster@extent@ymin) / raster@nrows,
                   (raster@extent@xmax - raster@extent@xmin) / raster@ncols)

  search_distance <- ceiling(search_distance / cell_size)

  search_distance
}

# flips matrix to fit the raster
# idea taken from https://github.com/gtatters/Thermimage/blob/master/R/mirror.matrix.R
.flip_matrix_horizontally <- function(x){
  as.matrix(rev(as.data.frame(x)))
}
