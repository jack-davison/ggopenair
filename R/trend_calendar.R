#' Plot time series values in a conventional calendar format
#'
#' With a single year of data, [trend_calendar()] will plot data in a conventional
#' calendar format, i.e., by month and day of the week. With multiple years of
#' data, a year-month matrix of panels will instead be plotted. Daily statistics
#' are calculated using [time_average()], which by default will calculate the
#' daily mean concentration.
#'
#' [trend_calendar()] has two accompanying annotation functions.
#' [annotate_calendar_text()] can write either the day of the month or the
#' average pollutant concentration on the calendar. [annotate_calendar_wd()]
#' will draw wind speed and direction arrows onto the calendar, assuming columns
#' labelled "ws" and "wd" were present in the original data.
#'
#' Note that is is possible to pre-calculate concentrations in some way before
#' passing the data to [trend_calendar()]. For example [openair::rollingMean()]
#' could be used to calculate rolling 8-hour mean concentrations. The data can
#' then be passed to [trend_calendar()] and \code{statistic = "max"} chosen, which
#' will plot maximum daily 8-hour mean concentrations.
#'
#' @param data A data frame minimally containing \code{date} and at least one
#'   other numeric variable. The date should be in either \code{Date} format or
#'   class \code{POSIXct}.
#' @param pollutant Mandatory. A pollutant name corresponding to a variable in a
#'   data frame should be supplied e.g. \code{pollutant = "nox".}
#' @param statistic Statistic passed to [time_average()].
#' @param data_thresh Data capture threshold passed to [time_average()]. For
#'   example, \code{data_thresh = 75} means that at least 75\% of the data must
#'   be available in a day for the value to be calculate, else the data is
#'   removed.
#' @param border_colour The colour to use for the border of each tile. Defaults
#'   to "white". `NA` removes the border.
#' @param w_shift Controls the order of the days of the week. By default the
#'   plot shows Saturday first (\code{w_shift = 0}). To change this so that it
#'   starts on a Monday for example, set \code{w_shift = 2}, and so on.
#' @export
#' @family time series and trend functions
#' @seealso [annotate_calendar_wd()] and [annotate_calendar_text()] for
#'   annotating calendar plots.
#' @examples
#' \dontrun{
#' marylebone %>%
#'   selectByDate(year = 2019) %>%
#'   trend_calendar("nox") +
#'   annotate_calendar_text(colour = "white", size = 5, type = "date") +
#'   annotate_calendar_wd(colour = "black")
#' }
trend_calendar <-
  function(data,
           pollutant,
           statistic = "mean",
           data_thresh = 0,
           border_colour = "white",
           w_shift = 0) {
    if (w_shift < 0 || w_shift > 6) {
      warning("w_shift should be between 0 and 6")
    }

    if (nrow(data) == 0) stop("No data to plot - check year chosen")

    ## all the days in the year
    all.dates <- seq(lubridate::as_date(lubridate::floor_date(min(data$date), "month")),
      lubridate::as_date(lubridate::ceiling_date(max(data$date), "month")) - 1,
      by = "day"
    )

    prepare.grid <- function(mydata, pollutant) {
      ## number of blank cells at beginning to get calendar format
      pad.start <- (as.numeric(format(mydata$date[1], "%w")) - w_shift) %% 7 + 1

      ## need to do in reverse to plot easily
      conc <- rev(mydata[[pollutant]])
      actual_date <- rev(mydata$date)

      ## day of the month
      theDates <- as.numeric(format(mydata$date, "%d"))
      theDates <- rev(theDates)

      daysAtEnd <- 42 - pad.start - nrow(mydata) ## 7x6 regular grid
      conc <- c(rep(NA, daysAtEnd), conc)

      actual_date <- c(rep(NA, daysAtEnd), actual_date)

      ## get relevant days in previous and next month, like a real calendar
      endDates <- mydata$date[nrow(mydata)] + (1:daysAtEnd)
      endDates <- rev(as.numeric(format(endDates, "%d")))

      theDates <- c(endDates, theDates)

      beginDates <- -1 * (1:pad.start) + mydata$date[1]
      beginDates <- as.numeric(format(beginDates, "%d"))

      conc <- c(conc, rep(NA, pad.start))

      actual_date <- c(actual_date, rep(NA, pad.start))

      if (pad.start != 0) theDates <- c(theDates, beginDates)

      ## convert to matrix
      conc.mat <- matrix(conc, ncol = 7, byrow = TRUE)
      date.mat <- matrix(theDates, ncol = 7, byrow = TRUE)
      actual_date.mat <- matrix(actual_date, ncol = 7, byrow = TRUE)

      ## need to reverse each row for calendar format
      conc.mat <- as.vector(apply(conc.mat, 1, rev))
      date.mat <- as.vector(apply(date.mat, 1, rev))
      actual_date.mat <- as.vector(apply(actual_date.mat, 1, rev))

      grid <- data.frame(expand.grid(x = 1:7, y = 1:6))
      results <- tibble(
        x = grid$x, y = grid$y, conc.mat,
        date.mat = date.mat,
        date = lubridate::as_date(actual_date.mat)
      )

      results
    }

    ## calculate daily means
    mydata <-
      time_average(
        data = data,
        avg_time = "day",
        statistic = statistic,
        data_thresh = data_thresh
      )

    mydata$date <- lubridate::as_date(mydata$date)

    # make sure all days are available
    mydata <- dplyr::left_join(dplyr::tibble(date = all.dates), mydata, by = "date")

    # split by year-month
    mydata <- dplyr::mutate(mydata,
      cuts = format(date, "%B-%Y"),
      cuts = ordered(.data$cuts, levels = unique(.data$cuts))
    )

    baseData <- mydata # for use later

    mydata <- mydata %>%
      dplyr::group_by(.data$cuts) %>%
      dplyr::do(prepare.grid(., pollutant)) %>%
      dplyr::ungroup()

    # combine data
    newdata <-
      dplyr::left_join(
        mydata, dplyr::select(baseData, dplyr::any_of(c("date", "ws", "wd"))),
        by = "date"
      ) %>%
      tidyr::separate(
        col = "cuts",
        into = c("month", "year"),
        sep = "-",
        convert = TRUE
      ) %>%
      dplyr::select(
        "date",
        "year",
        "month",
        "day" = "date.mat",
        "value" = "conc.mat",
        dplyr::any_of(c("wd", "ws")),
        "x",
        "y"
      ) %>%
      dplyr::mutate(month = factor(.data$month, month.name))

    plt <-
      ggplot2::ggplot(newdata) +
      ggplot2::geom_tile(
        ggplot2::aes(
          x = .data$x,
          y = .data$y,
          fill = .data$value
        ),
        colour = border_colour
      ) +
      ggplot2::scale_y_continuous(breaks = NULL, name = NULL) +
      ggplot2::scale_x_continuous(
        breaks = 1:7, name = NULL,
        labels = wshift(c("S", "S", "M", "T", "W", "T", "F"), w_shift)
      ) +
      ggplot2::coord_equal(expand = FALSE, clip = "off") +
      ggplot2::labs(fill = openair::quickText(pollutant)) +
      ggplot2::theme_minimal()

    if (dplyr::n_distinct(newdata$year) == 1) {
      plt <- plt +
        ggplot2::facet_wrap(ggplot2::vars(.data$month))
    } else {
      plt <- plt +
        ggplot2::facet_grid(
          rows = ggplot2::vars(.data$year),
          cols = ggplot2::vars(.data$month)
        )
    }

    plt
  }

#' Annotate a Calendar Plot with Text
#'
#' This function will add text to a [trend_calendar()] plot, which can either be
#' the day of the month (like an ordinary calendar) or the pollutant
#' concentration.
#'
#' @param type What type of annotation should be added? Can be one of "date"
#'   (the day of the month) or "value" (the pollutant concentration).
#' @param size The size of the annotation text.
#' @param colour The colour for the annotation text.
#' @param na_colour The colour for the annotation text for days outside of a
#'   given month. Used when `type = "date"`.
#' @param digits The number of decimal places used for the annotation text when
#'   `type = "value"`.
#' @param ... Other arguments passed to [ggplot2::geom_text()].
#' @family calendarplot annotation functions
#' @export
annotate_calendar_text <-
  function(type = "date",
           colour = "black",
           na_colour = "grey70",
           size = 3,
           digits = 0,
           ...) {
    type <- switch(type,
      date = "day",
      value = "value"
    )

    list(
      ggplot2::geom_text(
        data = function(x) {
          dplyr::filter(x, is.na(.data$value))
        },
        na.rm = TRUE,
        show.legend = FALSE,
        ggplot2::aes(
          x = .data$x,
          y = .data$y,
          label = round(.data[[type]], digits = digits)
        ),
        size = size,
        colour = na_colour,
        ...
      ),
      ggplot2::geom_text(
        data = function(x) {
          dplyr::filter(x, !is.na(.data$value))
        },
        na.rm = TRUE,
        show.legend = FALSE,
        ggplot2::aes(
          x = .data$x,
          y = .data$y,
          label = round(.data[[type]], digits = digits)
        ),
        size = size,
        colour = colour,
        ...
      )
    )
  }

#' Annotate a Calendar Plot with Wind Direction Arrows
#'
#' This function will add wind direction arrows to a [trend_calendar()] plot, sized
#' based on wind speed. Note that this function can only be used if the original
#' data contained numeric "ws" and "wd" columns.
#'
#' @param size The maximum size of the arrows.
#' @param colour The colour for the arrows.
#' @param ... Other arguments passed to [metR::geom_arrow()].
#' @family calendarplot annotation functions
#' @export
annotate_calendar_wd <- function(size = 1, colour = "black", ...) {
  list(
    metR::geom_arrow(
      na.rm = TRUE,
      colour = colour,
      start = -90,
      direction = "cw",
      ggplot2::aes(
        x = .data$x,
        y = .data$y,
        mag = .data$ws,
        angle = .data$wd
      ),
      ...
    ),
    metR::scale_mag(max_size = size),
    ggplot2::guides(mag = ggplot2::guide_none())
  )
}

#' shift vector around (for days of week)
#' @param x vector
#' @param n number
#' @noRd
wshift <- function(x, n = 0) {
  if (n == 0) x else c(utils::tail(x, -n), utils::head(x, n))
}
