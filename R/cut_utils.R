#' Discretise date-times into categories
#'
#' [cut_date()] conveniently extracts useful features from a vector of date-time
#' data, such as the year, season, whether or not it is a weekend, and so on.
#' This may be useful
#'
#' @param x date(time) vector
#' @param type The feature to extract from the vector. Can be any one of "year",
#'   "month", "hour", "yearmon", "weekday", "weekend", "season", or
#'   "seasonyear".
#' @param hemisphere Used to split data into seasons. Can be "northern" (the
#'   default) or "southern".
#'
#' @return a factor vector.
#' @export
#'
#' @examples
#' \dontrun{
#' # vector
#' cut_date(openair::mydata$date, "year")
#' # data frame
#' dplyr::mutate(openair::mydata, year = cut_date(year))
#' }
cut_date <-
  function(x,
           type,
           hemisphere = "northern") {
    type <-
      match.arg(
        arg = type,
        choices = c(
          "year",
          "month",
          "hour",
          "yearmon",
          "weekday",
          "weekend",
          "season",
          "seasonyear"
        )
      )

    hemisphere <-
      match.arg(
        arg = hemisphere,
        choices = c("northern", "southern")
      )

    if (type == "year") {
      x <- lubridate::year(x)
    }

    if (type == "month") {
      x <- lubridate::month(x, label = TRUE)
    }

    if (type == "hour") {
      x <- lubridate::hour(x)
    }

    if (type == "yearmon") {
      x <- zoo::as.yearmon(x)
    }

    if (type == "weekday") {
      x <- lubridate::wday(x, label = TRUE)
    }

    if (type == "weekend") {
      x <- lubridate::wday(x, label = TRUE)
      x <- ifelse(x %in% c("Sat", "Sun"), "Weekend", "Weekday")
    }

    if (type == "season") {
      x <- cut_season(x, hemisphere)
    }

    if (type == "seasonyear") {
      a <- cut_season(x, hemisphere)
      b <- lubridate::year(x)
      x <- paste(a, b)
    }

    if (!is.factor(x)) x <- factor(x)

    return(x)

  }

#' Discretise wind speed into sectors
#'
#' [cut_wd()] cuts numeric wind speed into sectors. By default, the function
#' splits wind direction into 8 sectors ("N", "NE", "E", "SE", "S", "SW", "W",
#' "NW"), it can also be split into either 4 or 16.
#'
#' @param x numeric vector representing wind speed, where `0` is North.
#' @param resolution A character string representing the number of sectors to
#'   split wind direction into. Can be `0
#'
#' @return a character vector.
#' @export
#'
#' @examples
#' \dontrun{
#' # vector
#' cut_wd(openair::mydata$wd)
#' # data frame
#' dplyr::mutate(openair::mydata, wd_bin = cut_wd(wd))
#' }
cut_wd <- function(x, resolution = "medium"){

  res <-
    switch(
      resolution,
      high = 22.5,
      medium = 45,
      low = 90
    )

  labs <-
    switch(
      resolution,
      high = c("N", "NNE", "NE", "ENE", "E", "ESE",
               "SE", "SSE", "S", "SSW", "SW", "WSW",
               "W", "WNW", "NW", "NNW", "N"),
      medium = c("N", "NE", "E", "SE", "S", "SW", "W", "NW", "N"),
      low = c("N", "E", "S", "W", "N")
    )

  breaks <- c(0, seq(0, 360, res) + res/2)

  y <- cut(
    x,
    breaks = breaks,
    labels = labs
  )

  y[x == 0] <- "N"

  return(y)

}

#' cut season helper function
#' @param x vector
#' @param hemisphere hemisphere
#' @noRd
cut_season <- function(x, hemisphere) {
  x <- lubridate::month(x, label = TRUE)
  if (hemisphere == "northern") {
    dplyr::case_when(
      x %in% c("Dec", "Jan", "Feb") ~ "Winter (DJF)",
      x %in% c("Mar", "Apr", "May") ~ "Spring (MAM)",
      x %in% c("Jun", "Jul", "Aug") ~ "Summer (JJA)",
      x %in% c("Sep", "Oct", "Nov") ~ "Autumn (SON)"
    )
  } else {
    dplyr::case_when(
      x %in% c("Dec", "Jan", "Feb") ~ "Summer (DJF)",
      x %in% c("Mar", "Apr", "May") ~ "Autumn (MAM)",
      x %in% c("Jun", "Jul", "Aug") ~ "Winter (JJA)",
      x %in% c("Sep", "Oct", "Nov") ~ "Spring (SON)"
    )
  }

}
