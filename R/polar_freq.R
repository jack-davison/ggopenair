#' Function to plot wind speed/direction frequencies and other statistics
#'
#' @inheritParams polar_plot
#' @param pollutant A column name identifying pollutant concentration.
#' @param statistic The statistic that should be applied to each wind
#'   speed/direction bin. Can be \dQuote{frequency}, \dQuote{mean},
#'   \dQuote{median}, \dQuote{max} (maximum), \dQuote{stdev} (standard
#'   deviation) or \dQuote{weighted.mean}. The option \dQuote{frequency} (the
#'   default) is the simplest and plots the frequency of wind speed/direction in
#'   different bins. The scale therefore shows the counts in each bin. The
#'   option \dQuote{mean} will plot the mean concentration of a pollutant (see
#'   next point) in wind speed/direction bins, and so on.  Finally,
#'   \dQuote{weighted.mean} will plot the concentration of a pollutant weighted
#'   by wind speed/direction. Each segment therefore provides the percentage
#'   overall contribution to the total concentration. More information is given
#'   in the examples. Note that for options other than \dQuote{frequency}, it is
#'   necessary to also provide the name of a pollutant. See function
#'   \code{cutData} for further details.
#' @param ws_int Wind speed interval assumed. In some cases e.g. a low met mast,
#'   an interval of 0.5 may be more appropriate.
#' @param wd_nint Number of intervals of wind direction.
#' @param border_colour The colour to use for the border of each tile. Defaults
#'   to `NA`, which removes the border.
#' @export
#' @family polar directional analysis functions
polar_freq <-
  function(data,
           pollutant = NULL,
           statistic = "frequency",
           ws_int = 1,
           wd_nint = 36,
           facet = NULL,
           min_bin = 1,
           border_colour = NA,
           alpha = 1) {
    if (is.null(facet)) facet <- "default"
    if (!is.null(pollutant)) {
      oa_data <-
        openair::polarFreq(
          mydata = data,
          pollutant = pollutant,
          statistic = statistic,
          ws.int = ws_int,
          wd.nint = wd_nint,
          type = facet,
          min.bin = min_bin,
          plot = FALSE,
          trans = FALSE
        )$data
    } else {
      oa_data <-
        openair::polarFreq(
          mydata = data,
          statistic = statistic,
          ws.int = ws_int,
          wd.nint = wd_nint,
          type = facet,
          min.bin = min_bin,
          plot = FALSE,
          trans = FALSE
        )$data
    }

    plot_data <-
      oa_data %>%
      dplyr::filter(.data$wd != 0) %>%
      dplyr::mutate(
        wd = .data$wd - (360 / wd_nint),
        wd = dplyr::if_else(.data$wd == max(.data$wd),
          0, .data$wd + (360 / wd_nint)
        ),
        ws = .data$ws - (ws_int / 2)
      )

    plt <-
      ggplot2::ggplot(plot_data, ggplot2::aes(x = .data$wd, y = .data$ws)) +
      ggplot2::geom_tile(
        alpha = alpha, colour = border_colour,
        ggplot2::aes(fill = .data$weights)
      ) +
      ggplot2::coord_polar(start = -pi / wd_nint) +
      ggplot2::expand_limits(y = -2.5) +
      ggplot2::scale_x_continuous(
        breaks = c(0, 90, 180, 270),
        labels = c("N", "E", "S", "W")
      ) +
      ggplot2::labs(x = NULL, y = NULL, fill = openair::quickText(pollutant))

    # sort out type
    facet <- dplyr::group_vars(plot_data)
    if (any(facet != "default")) {
      if (length(facet) == 1) {
        plt <-
          plt + ggplot2::facet_wrap(facets = ggplot2::vars(.data[[facet]]))
      } else {
        plt <-
          plt + ggplot2::facet_grid(
            cols = ggplot2::vars(.data[[facet[1]]]),
            rows = ggplot2::vars(.data[[facet[2]]])
          )
      }
    }

    plt
  }
