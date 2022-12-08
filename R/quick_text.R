#' Automatic text formatting for ggopenair
#'
#' [quick_text()] is routine formatting lookup table which automatically
#' prepares common pollutant names and units for parsing using
#' [ggplot2::label_parsed()] or similar. It is automatically called by several
#' \code{ggopenair} functions. Unlike [openair::quickText()], [quick_text()]
#' returns a character vector, which means its output can be stored in a data
#' frame or [tibble::tibble()]
#'
#' @param text A character vector.
#' @export
#' @return A character vector.

quick_text <- function(text) {

  # pollutants
  text <- gsub("NO2|no2|No2", "NO[2]", text)
  text <- gsub("\\bnox\\b|\\bNOx\\b|\\bNox\\b|\\bNOX\\b", "NO[x]", text)
  text <- gsub("CO2|co2|Co2", "CO[2]", text)
  text <- gsub("co", "CO", text)
  text <- gsub("o3|O3|ozone", "O[3]", text)
  text <- gsub("SO2|so2|So2", "SO[2]", text)
  text <- gsub("\\bno\\b|\\bNO\\b|\\bNo\\b", "NO", text)
  text <- gsub("pm25|PM25|pm25|pm2.5|PM2.5|Pm2.5", "PM[2.5]", text)
  text <- gsub("pm10|PM10|Pm10", "PM[10]", text)
  text <- gsub("pm1|PM1|Pm1", "PM[1]", text)
  text <- gsub("pm4|PM4|Pm4", "PM[4]", text)
  text <- gsub("pmtot", "PM[4]", text, ignore.case = TRUE)
  text <- gsub("pmc|pmcoarse", "PM[coarse]", text, ignore.case = TRUE)
  text <- gsub("pmf|pmfine", "PM[fine]", text, ignore.case = TRUE)
  text <- gsub("nh3|NH3|Nh3", "NH[3]", text)
  text <- gsub("nv10|NV10|Nv10", "NV[10]", text)
  text <- gsub("v10|V10", "V[10]", text)
  text <- gsub("nv25|NV25|Nv25|nv2.5|NV2.5|Nv2.5", "NV[2.5]", text)
  text <- gsub("v25|V25|v2.5|V2.5", "V[2.5]", text)
  text <- gsub("nmhc", "NMHC", text)
  text <- gsub("h2s", "H[2]S", text, ignore.case = TRUE)
  text <- gsub("ch4", "CH[4]", text, ignore.case = TRUE)

  # units
  text <- gsub("ug/m3", "\u00B5g m^{-3}", text)
  text <- gsub("m/s", "m s^{-1}", text)

  # other
  text <- gsub("\\bwd\\b", "Wind dir.", text)
  text <- gsub("\\bws\\b", "Wind spd.", text)
  text <- gsub("\\brh\\b", "Relative humidity", text)
  text <- gsub("r2", "r^{2}", text, ignore.case = TRUE)

  # fix spaces
  text <- gsub(" ", "~", text)

  # ans <- gsub("dgrC", "' * degree * 'C' '", ans)
  # ans <- gsub("degreeC", "' * degree * 'C' '", ans)
  # ans <- gsub("deg. C", "' * degree * 'C' '", ans)
  # ans <- gsub("degreesC", "' * degree * 'C' '", ans)
  # ans <- gsub("degrees", "' * degree *'", ans)
  # ans <- gsub("Delta", "' * Delta *'", ans)
  # ans <- gsub("delta", "' * Delta *'", ans)
  # ans <- gsub("ug/m3", "' * mu * 'g m' ^-3 *'", ans)
  # ans <- gsub("ug.m-3", "' * mu * 'g m' ^-3 *'", ans)
  # ans <- gsub("ug m-3", "' * mu * 'g m' ^-3 *'", ans)
  # ans <- gsub("ugm-3", "' * mu * 'g m' ^-3 *'", ans)
  # ans <- gsub("mg/m3", "' * 'm' * 'g m' ^-3 *'", ans)
  # ans <- gsub("mg.m-3", "' * 'm' * 'g m' ^-3 *'", ans)
  # ans <- gsub("mg m-3", "' * 'm' * 'g m' ^-3 *'", ans)
  # ans <- gsub("mgm-3", "' * 'm' * 'g m' ^-3 *'", ans)
  # ans <- gsub("ng/m3", "' * 'n' * 'g m' ^-3 *'", ans)
  # ans <- gsub("ng.m-3", "' * 'n' * 'g m' ^-3 *'", ans)
  # ans <- gsub("ng m-3", "' * 'n' * 'g m' ^-3 *'", ans)
  # ans <- gsub("ngm-3", "' * 'n' * 'g m' ^-3 *'", ans)
  # ans <- gsub("m/s2", "' 'm s' ^-2 *'", ans)
  # ans <- gsub("m/s", "' 'm s' ^-1 *'", ans)
  # ans <- gsub("m.s-1", "' 'm s' ^-1 *'", ans)
  # ans <- gsub("m s-1", "' 'm s' ^-1 *'", ans)
  # ans <- gsub("g/km", "' 'g km' ^-1 *'", ans)
  # ans <- gsub("g/s", "' 'g s' ^-1 *'", ans)
  # ans <- gsub("kW/t", "' 'kW t' ^-1 *'", ans)
  # ans <- gsub("g/hour", "' 'g hour' ^-1 *'", ans)
  # ans <- gsub("g/hr", "' 'g hour' ^-1 *'", ans)
  # ans <- gsub("g/m3", "' 'g m' ^-3 *'", ans)
  # ans <- gsub("g/kg", "' 'g kg' ^-1 *'", ans)
  # ans <- gsub("km/hr/s", "' 'km hr' ^-1 * ' s' ^-1 *'", ans)
  # ans <- gsub("km/hour/s", "' 'km hr' ^-1 * ' s' ^-1 *'", ans)
  # ans <- gsub("km/h/s", "km hr' ^-1 * ' s' ^-1 *'", ans)
  # ans <- gsub("km/hr", "' 'km hr' ^-1 *'", ans)
  # ans <- gsub("km/h", "' 'km hr' ^-1 *'", ans)
  # ans <- gsub("km/hour", "' 'km hr' ^-1 *'", ans)
  # ans <- gsub("umol/m2/s", "' * mu * 'mol m' ^-2 * ' s' ^-1 *'", ans)
  # ans <- gsub("umol/m2", "' * mu * 'mol m' ^-2 *'", ans)

  text
}
