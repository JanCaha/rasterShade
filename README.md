# rasterShade

The package rasterShade is just a wrapper around function in [rayshader](https://github.com/tylermorganwall/rayshader) package. The rasterShade package allows using rayshader algorithm directly on spatial data in form of Raster. The package tries to set "reasonable" default values for parameters in rayshadering algorithm. These default values are based on real world shadows that can be seen in nature.

## Installation

First you need to install [rayshader](https://github.com/tylermorganwall/rayshader) package from GitHub.

``` r
devtools::install_github("tylermorganwall/rayshader")
```

Then this package *rasterShade* can be installed:

``` r
devtools::install_github("JanCaha/rasterShade")
```

## Example

The simple example showing calculation of global shadows on digital surface model:

``` r
raster <- raster("digital_surface_model.tif")
shadow_raster <- shade(raster, date = "2018-05-23", sun_elevation = "noon", search_distance = 100)
```

