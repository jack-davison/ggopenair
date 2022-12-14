#' Hourly Air Quality and Met Data for Marylebone Road, London
#'
#' [marylebone] contains hourly pollutant concentrations for five air quality
#' species, as well as modelled wind speed, wind direction and air temperature.
#' This data frame is in an appropriate structure for use in `ggopenair`, and
#' can act as a template for users wanting to use their own data.
#'
#' @source These data were obtained from AURN site "my1" using
#'   [openair::importAURN()].
#'
#' @format A data frame with 52,608 rows and 9 variables:
#' \describe{
#'   \item{date}{Observation date/time stamp in the POSIXct time format.}
#'   \item{ws}{Wind speed, in m/s.}
#'   \item{wd}{Wind direction, in degrees from North.}
#'   \item{air_temp}{Air temperature, in degrees Celcius.}
#'   \item{nox, no2, o3, pm2.5, pm10}{Pollutant concentrations in ug/m3.}
#' }
"marylebone"
