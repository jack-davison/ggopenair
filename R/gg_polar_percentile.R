#' Function to plot percentiles by wind direction
#'
#' @param data A data frame minimally containing \code{wd} and a
#'   numeric field to plot --- \code{pollutant}.
#' @param pollutant Mandatory. A pollutant name corresponding to a
#'   variable in a data frame should be supplied e.g. \code{pollutant
#'   = "nox"}. More than one pollutant can be supplied e.g.
#'   \code{pollutant = c("no2", "o3")} provided there is only one
#'   \code{type}.
#' @param type \code{type} determines how the data are split i.e.
#'   conditioned, and then plotted. The default is will produce a
#'   single plot using the entire data. Type can be one of the
#'   built-in types as detailed in \code{cutData} e.g.
#'   \dQuote{season}, \dQuote{year}, \dQuote{weekday} and so on. For
#'   example, \code{type = "season"} will produce four plots --- one
#'   for each season.
#'
#'   It is also possible to choose \code{type} as another variable in
#'   the data frame. If that variable is numeric, then the data will
#'   be split into four quantiles (if possible) and labelled
#'   accordingly. If type is an existing character or factor
#'   variable, then those categories/levels will be used directly.
#'   This offers great flexibility for understanding the variation of
#'   different variables and how they depend on one another.
#'
#'   Type can be up length two e.g. \code{type = c("season",
#'   "weekday")} will produce a 2x2 plot split by season and day of
#'   the week. Note, when two types are provided the first forms the
#'   columns and the second the rows.
#' @param percentile The percentile value(s) to plot. Must be between
#'   0--100. If \code{percentile = NA} then only a mean line will be
#'   shown.
#' @param method When \code{method = "default"} the supplied
#'   percentiles by wind direction are calculated. When \code{method
#'   = "cpf"} the conditional probability function (CPF) is plotted
#'   and a single (usually high) percentile level is supplied. The
#'   CPF is defined as CPF = my/ny, where my is the number of samples
#'   in the wind sector y with mixing ratios greater than the
#'   \emph{overall} percentile concentration, and ny is the total
#'   number of samples in the same wind sector (see Ashbaugh et al.,
#'   1985).
#' @param line_lty Line type for the percentile lines.
#' @param line_width Line width for the percentile lines.
#' @param mean Show the mean by wind direction as a line?
#' @param mean_lty Line type for mean line.
#' @param mean_width Line width for mean line.
#' @param mean_colour Line colour for mean line.
#' @export

gg_polar_percentile <- function(data, pollutant,
                                type = "default",
                                percentile = c(25, 50, 75, 90, 95),
                                method = "default",
                                line_lty = 1,
                                line_width = 1,
                                mean = TRUE,
                                mean_lty = 1,
                                mean_width = 1,
                                mean_colour = "grey") {
  # run openair
  oa_data <- openair::percentileRose(
    mydata = data,
    pollutant = pollutant,
    type = type,
    percentile = percentile,
    method = method,
    plot = FALSE
  )$data

  plot_dat <-
    dplyr::filter(
      oa_data,
      .data$percentile != 0,
      .data$wd != 365
    )

  plt <-
    plot_dat %>%
    dplyr::filter(percentile != 999) %>%
    ggplot2::ggplot(ggplot2::aes(x = .data$wd, y = .data[[pollutant]])) +
    ggplot2::geom_step(ggplot2::aes(color = factor(percentile)),
      linewidth = line_width,
      lty = line_lty
    ) +
    ggplot2::coord_polar(start = -pi / 36) +
    ggplot2::scale_x_continuous(
      breaks = c(0, 90, 180, 270),
      labels = c("N", "E", "S", "W")
    ) +
    ggplot2::expand_limits(y = 0) +
    ggplot2::labs(x = NULL, y = NULL, color = "Percentile")

  if (mean) {
    plt <- plt +
      ggplot2::geom_step(
        data = dplyr::filter(plot_dat, percentile == 999),
        colour = mean_colour,
        linewidth = mean_width,
        lty = mean_lty
      )
  }

  # sort out type
  if (any(type != "default")) {
    if (length(type) == 1) {
      plt <-
        plt + ggplot2::facet_wrap(facets = ggplot2::vars(.data[[type]]))
    } else {
      plt <-
        plt + ggplot2::facet_grid(
          cols = ggplot2::vars(.data[[type[1]]]),
          rows = ggplot2::vars(.data[[type[2]]])
        )
    }
  }

  plt
}
