#' Function to plot percentiles by wind direction
#'
#' @inheritParams polar_plot
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
#' @param line_lty Line type for the percentile lines (see [ggplot2::linetype]).
#' @param line_width Line width for the percentile lines.
#' @param mean Show the mean by wind direction as a line?
#' @param mean_lty Line type for mean line (see [ggplot2::linetype]).
#' @param mean_width Line width for mean line.
#' @param mean_colour Line colour for mean line.
#' @export
#' @family polar directional analysis functions
rose_percentile <- function(data, pollutant,
                             facet = NULL,
                             percentile = c(25, 50, 75, 90, 95),
                             method = "default",
                             line_lty = 1,
                             line_width = 1,
                             mean = TRUE,
                             mean_lty = 1,
                             mean_width = 1,
                             mean_colour = "grey",
                             alpha = 1) {
  # run openair
  if (is.null(facet)) facet <- "default"
  oa_data <- openair::percentileRose(
    mydata = data,
    pollutant = pollutant,
    type = facet,
    percentile = percentile,
    method = method,
    plot = FALSE
  )$data

  plot_data <-
    dplyr::filter(
      oa_data,
      .data$percentile != 0,
      .data$wd != 365
    )

  plt <-
    plot_data %>%
    dplyr::filter(.data$percentile != 999) %>%
    ggplot2::ggplot(ggplot2::aes(x = .data$wd, y = .data[[pollutant]])) +
    ggplot2::geom_step(ggplot2::aes(color = factor(percentile)),
      linewidth = line_width,
      lty = line_lty,
      alpha = alpha
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
        data = dplyr::filter(plot_data, .data$percentile == 999),
        colour = mean_colour,
        linewidth = mean_width,
        lty = mean_lty
      )
  }

  # sort out type
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
