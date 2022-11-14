#' Function to plot wind speed/direction frequencies and other statistics
#'
#' @param data A data frame minimally containing \code{ws}, \code{wd} and
#'   \code{date}.
#' @param pollutant Mandatory. A pollutant name corresponding to a variable in
#'   a data frame should be supplied e.g. \code{pollutant = "nox"}
#' @param statistic The statistic that should be applied to each wind
#' speed/direction bin. Can be \dQuote{frequency}, \dQuote{mean},
#' \dQuote{median}, \dQuote{max} (maximum), \dQuote{stdev} (standard
#' deviation) or \dQuote{weighted.mean}. The option
#' \dQuote{frequency} (the default) is the simplest and plots the
#' frequency of wind speed/direction in different bins. The scale
#' therefore shows the counts in each bin. The option \dQuote{mean}
#' will plot the mean concentration of a pollutant (see next point)
#' in wind speed/direction bins, and so on.  Finally,
#' \dQuote{weighted.mean} will plot the concentration of a pollutant
#' weighted by wind speed/direction. Each segment therefore provides
#' the percentage overall contribution to the total concentration.
#' More information is given in the examples. Note that for options
#' other than \dQuote{frequency}, it is necessary to also provide the
#' name of a pollutant. See function \code{cutData} for further
#' details.
#' @param ws_int Wind speed interval assumed. In some cases e.g. a low met
#'   mast, an interval of 0.5 may be more appropriate.
#' @param wd_nint Number of intervals of wind direction.
#' @param type \code{type} determines how the data are split
#' i.e. conditioned, and then plotted. The default is will produce a
#' single plot using the entire data. Type can be one of the built-in
#' types as detailed in \code{cutData} e.g. \dQuote{season},
#' \dQuote{year}, \dQuote{weekday} and so on. For example, \code{type
#' = "season"} will produce four plots --- one for each season.
#'
#' It is also possible to choose \code{type} as another variable in
#' the data frame. If that variable is numeric, then the data will be
#' split into four quantiles (if possible) and labelled
#' accordingly. If type is an existing character or factor variable,
#' then those categories/levels will be used directly. This offers
#' great flexibility for understanding the variation of different
#' variables and how they depend on one another.
#'
#' Type can be up length two e.g. \code{type = c("season", "weekday")} will
#'   produce a 2x2 plot split by season and day of the week. Note, when two
#'   types are provided the first forms the columns and the second the rows.
#' @param min_bin The minimum number of points allowed in a wind speed/wind
#'   direction bin.  The default is 1. A value of two requires at least 2
#'   valid records in each bin an so on; bins with less than 2 valid records
#'   are set to NA. Care should be taken when using a value > 1 because of the
#'   risk of removing real data points. It is recommended to consider your
#'   data with care. Also, the \code{polarPlot} function can be of use in such
#'   circumstances.
#' @param border_colour The colour to use for the border of each tile. Defaults
#'   to `NA`, which removes the border.
#' @export

gg_polar_freq <-
  function(data,
           pollutant = NULL,
           statistic = "frequency",
           ws_int = 1,
           wd_nint = 36,
           type = "default",
           min_bin = 1,
           border_colour = NA) {

    if (!is.null(pollutant)){
      oa_data <-
        openair::polarFreq(
          mydata = data,
          pollutant = pollutant,
          statistic = statistic,
          ws.int = ws_int,
          wd.nint = wd_nint,
          type = type,
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
          type = type,
          min.bin = min_bin,
          plot = FALSE,
          trans = FALSE
        )$data
    }

  plt <-
    oa_data %>%
    dplyr::filter(.data$wd != 0) %>%
    dplyr::mutate(wd = .data$wd - (360/wd_nint)) %>%
    ggplot2::ggplot(ggplot2::aes(x = .data$wd, y = .data$ws)) +
    ggplot2::geom_tile(colour = border_colour, ggplot2::aes(fill = .data$weights)) +
    ggplot2::coord_polar(start = -pi / wd_nint) +
    ggplot2::expand_limits(y = -2.5) +
    ggplot2::scale_x_continuous(breaks = c(0, 90, 180, 270),
                                labels = c("N", "E", "S", "W")) +
    ggplot2::labs(x = NULL, y = NULL, fill = openair::quickText(pollutant))

  if (any(type != "default")) {
    if (length(type) == 1) {
      plt <-
        plt + ggplot2::facet_wrap(facets = ggplot2::vars(.data[[type]]))
    } else {
      plt <-
        plt + ggplot2::facet_grid(cols = ggplot2::vars(.data[[type[1]]]),
                                  rows = ggplot2::vars(.data[[type[2]]]))
    }
  }

  plt

  }

