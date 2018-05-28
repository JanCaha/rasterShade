# according to https://www.esrl.noaa.gov/gmd/grad/solcalc/calcdetails.html
# file 	NOAA_Solar_Calculations_day.xls
# this page also helps https://www.suncalc.org/#/50.0906,14.4004,15/2018.06.21/13:48/2/0
.calculate_atmospheric_refraction <- function(sun_elevation_angle){

  refraction <- 0
  sun_elevation_rad <- sun_elevation_angle * (pi/180)

  if (sun_elevation_angle > 85) {
    refraction <- 0
  } else if (sun_elevation_angle > 5) {
    refraction <- 58.1/tan(sun_elevation_rad) - 0.07/(tan(sun_elevation_rad)^3) + 0.000086/(tan(sun_elevation_rad)^5)
  } else if (sun_elevation_angle > -0.575) {
    refraction <- 1735 + sun_elevation_angle*(-518.2 + sun_elevation_angle*(103.4 + sun_elevation_angle*(-12.79 + sun_elevation_angle*0.711)))
  } else {
    refraction <- (-20.772)/(tan(sun_elevation_rad))
  }

  refraction <- refraction/3600

  return(refraction)
}
