# rasterShade

<!-- badges: start -->
![R-CMD-check](https://github.com/JanCaha/rasterShade/workflows/R-CMD-check/badge.svg)
<!-- badges: end -->

The package rasterShade is just a wrapper around function in [rayshader](https://github.com/tylermorganwall/rayshader) package. The rasterShade package allows using rayshader algorithm directly on spatial data in form of Raster. The package tries to set "reasonable" default values for parameters in rayshadering algorithm. These default values are based on real world shadows that can be seen in nature.

## Installation

First you need to install [rayshader](https://github.com/tylermorganwall/rayshader) package from CRAN or GitHub. The description of this package can be found in this [great blogpost](http://www.tylermw.com/throwing-shade/) and in this [follow-up](http://www.tylermw.com/making-beautiful-maps/).

``` r
devtools::install_github("tylermorganwall/rayshader")
```

Then this package **rasterShade** can be installed:

``` r
devtools::install_github("JanCaha/rasterShade")
```

## Example

Loading the libraries and preparing data:

``` r
library(raster)
library(rasterShade)

raster <- raster("digital_surface_model.tif")
```

Global shading raster:

``` r
shadow_raster <- shade_global_date_time(raster, 
                                        date = "2018-05-23", 
                                        sun_elevation = "noon", 
                                        search_distance = 100)
                              
writeRaster(shadow_raster, "equinox_noon.tif", overwrite = TRUE)
```

Raster shading using lambert method:

``` r
shadow_raster <- shade_global_date_time(raster, 
                                        only_facing_sun = TRUE, 
                                        verbose = TRUE)

writeRaster(shadow_raster, "today_noon_lambert.tif", overwrite = TRUE)
```

Raster shading using ambient shadows:

``` r
shadow_raster <- shade_ambient_date_time(raster, 
                                        date = "2018-05-23", 
                                        sun_elevation = "noon", 
                                        rays_number = 36, 
                                        search_distance = 100)
                               
writeRaster(shadow_raster, "equinox_noon_ambient.tif", overwrite = TRUE)
```

