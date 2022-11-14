#' Plot time series values in convential calendar format
#'
#' @param data A data frame minimally containing \code{date} and at least one
#'   other numeric variable. The date should be in either \code{Date} format or
#'   class \code{POSIXct}.
#' @param pollutant Mandatory. A pollutant name corresponding to a variable in a
#'   data frame should be supplied e.g. \code{pollutant = "nox".}
#' @param statistic Statistic passed to [openair::timeAverage()].
#' @param data_thresh Data capture threshold passed to [openair::timeAverage()].
#'   For example, \code{data_thresh = 75} means that at least 75\% of the data
#'   must be available in a day for the value to be calculate, else the data is
#'   removed.
#' @param border_colour The colour to use for the border of each tile. Defaults
#'   to "white". `NA` removes the border.
#' @param w_shift Controls the order of the days of the week. By default the
#'   plot shows Saturday first (\code{w_shift = 0}). To change this so that it
#'   starts on a Monday for example, set \code{w_shift = 2}, and so on.
#' @export
#' @examples
#' \dontrun{
#' marylebone %>%
#'   selectByDate(year = 2019) %>%
#'   gg_calendar("nox") +
#'   annotate_calendar_text(colour = "white", size = 5, type = "date") +
#'   annotate_calendar_wd(colour = "black")
#' }
gg_calendar <-
  function(data,
           pollutant,
           statistic = "mean",
           data_thresh = 0,
           border_colour = "white",
           w_shift = 0) {
    # do annotate if the data is available
    if ("ws" %in% names(data)) {
      ann <- "ws"
    } else {
      ann <- "value"
    }

    # openair data
    oa_data <-
      openair::calendarPlot(
        mydata = data,
        pollutant = pollutant,
        statistic = statistic,
        data.thresh = data_thresh,
        w.shift = w_shift,
        annotate = ann,
        plot = FALSE
      )$data

    plot_data <-
      tidyr::separate(oa_data,
        .data$cuts,
        c("month", "year"),
        sep = "-",
        convert = TRUE
      ) %>%
      dplyr::mutate(month = forcats::fct_reorder(.data$month, match(.data$month, month.name)))

    plt <-
      ggplot2::ggplot(plot_data) +
      ggplot2::geom_tile(ggplot2::aes(
        x = .data$x,
        y = .data$y,
        fill = .data$conc.mat
      ),
      colour = border_colour
      ) +
      ggplot2::scale_y_continuous(breaks = NULL, name = NULL) +
      ggplot2::scale_x_continuous(
        breaks = 1:7, name = NULL,
        labels = c("S", "S", "M", "T", "W", "T", "F")
      ) +
      ggplot2::coord_equal(expand = F, clip = "off") +
      ggplot2::labs(fill = openair::quickText(pollutant)) +
      ggplot2::theme_minimal()

    if (dplyr::n_distinct(plot_data$year) == 1) {
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
#' This function will add text to a [gg_calendar()] plot, which can either be
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
#' @export
annotate_calendar_text <-
  function(type = "date",
           colour = "black",
           na_colour = "grey70",
           size = 3,
           digits = 0,
           ...) {
    type <- switch(type,
      date = "date.mat",
      value = "conc.mat"
    )

    list(
      ggplot2::geom_text(
        data = function(x) {
          dplyr::filter(x, is.na(.data$conc.mat))
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
          dplyr::filter(x, !is.na(.data$conc.mat))
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
#' This function will add wind direction arrows to a [gg_calendar()] plot, sized
#' based on wind speed. Note that this function can only be used if the original
#' data contained numeric "ws" and "wd" columns.
#'
#' @param size The maximum size of the arrows.
#' @param colour The colour for the arrows.
#' @param ... Other arguments passed to [metR::geom_arrow()].
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
