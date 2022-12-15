#' Bivariate polarAnnulus plot
#'
#' Typically plots the concentration of a pollutant by wind direction and as a
#' function of time as an annulus. The function is good for visualising how
#' concentrations of pollutants vary by wind direction and a time period, e.g.,
#' by month, day of week, etc.
#'
#' [polar_annulus()] shares many of the properties of the [polar_plot()].
#' However, [polar_annulus()] is focussed on displaying information on how
#' concentrations of a pollutant (values of another variable) vary with wind
#' direction and time. Plotting as an annulus helps to reduce compression of
#' information towards the centre of the plot. The circular plot is easy to
#' interpret because wind direction is most easily understood in polar rather
#' than Cartesian coordinates.
#'
#' The inner part of the annulus represents the earliest time and the outer part
#' of the annulus the latest time. The time dimension can be shown in many ways
#' including "trend", "hour" (hour or day), "season" (month of the year) and
#' "weekday" (day of the week). Taking hour as an example, the plot will show
#' how concentrations vary by hour of the day and wind direction. Such plots can
#' be very useful for understanding how different source influences affect a
#' location.
#'
#' For \code{type = "trend"} the amount of smoothing does not vary linearly with
#' the length of the time series, i.e., a certain amount of smoothing per unit
#' interval in time. This is a deliberate choice because should one be
#' interested in a subset (in time) of data, more detail will be provided for
#' the subset compared with the full data set. This allows users to investigate
#' specific periods in more detail. Full flexibility is given through the
#' smoothing parameter \code{k}.
#'
#' @inheritParams polar_plot
#'
#' @param local_tz Should the results be calculated in local time that includes
#'   a treatment of daylight savings time (DST)? The default is not to consider
#'   DST issues, provided the data were imported without a DST offset. Emissions
#'   activity tends to occur at local time e.g. rush hour is at 8 am every day.
#'   When the clocks go forward in spring, the emissions are effectively
#'   released into the atmosphere typically 1 hour earlier during the summertime
#'   i.e. when DST applies. When plotting diurnal profiles, this has the effect
#'   of \dQuote{smearing-out} the concentrations. Sometimes, a useful approach
#'   is to express time as local time. This correction tends to produce
#'   better-defined diurnal profiles of concentration (or other variables) and
#'   allows a better comparison to be made with emissions/activity data. If set
#'   to \code{FALSE} then GMT is used. Examples of usage include \code{local_tz
#'   = "Europe/London"}, \code{local_tz = "America/New_York"}. See
#'   \code{cutData} and \code{import} for more details.
#' @param period This determines the temporal period to consider. Options are
#'   \dQuote{hour} (the default, to plot diurnal variations), \dQuote{season} to
#'   plot variation throughout the year, \dQuote{weekday} to plot day of the
#'   week variation and \dQuote{trend} to plot the trend by wind direction.
#' @param statistic The statistic that should be applied to each wind
#'   speed/direction bin. Can be \dQuote{mean} (default), \dQuote{median},
#'   \dQuote{max} (maximum), \dQuote{frequency}. \dQuote{stdev} (standard
#'   deviation), \dQuote{weighted.mean} or \dQuote{cpf} (Conditional Probability
#'   Function). Because of the smoothing involved, the colour scale for some of
#'   these statistics is only to provide an indication of overall pattern and
#'   should not be interpreted in concentration units e.g. for \code{statistic =
#'   "weighted.mean"} where the bin mean is multiplied by the bin frequency and
#'   divided by the total frequency. In many cases using \code{polarFreq} will
#'   be better. Setting \code{statistic = "weighted.mean"} can be useful because
#'   it provides an indication of the concentration * frequency of occurrence
#'   and will highlight the wind speed/direction conditions that dominate the
#'   overall mean.
#' @param percentile If \code{statistic = "percentile"} or \code{statistic =
#'   "cpf"} then \code{percentile} is used, expressed from 0 to 100. Note that
#'   the percentile value is calculated in the wind speed, wind direction
#'   \sQuote{bins}. For this reason it can also be useful to set \code{min_bin}
#'   to ensure there are a sufficient number of points available to estimate a
#'   percentile. See \code{quantile} for more details of how percentiles are
#'   calculated.
#' @param width The relative width of the annulus compared to the width of the
#'   inner white space. `width = 2` makes the annulus twice as wide as the inner
#'   circle, whereas `width = 0.5` makes the annulus half as wide as the inner
#'   circle. Defaults to `1`.
#' @param exclude_missing Setting this option to \code{TRUE} (the default)
#'   removes points from the plot that are too far from the original data. The
#'   smoothing routines will produce predictions at points where no data exist
#'   i.e. they predict. By removing the points too far from the original data
#'   produces a plot where it is clear where the original data lie. If set to
#'   \code{FALSE} missing data will be interpolated.
#' @param pad_date For \code{type = "trend"} (default), \code{pad_date = TRUE}
#'   will pad-out missing data to the beginning of the first year and the end of
#'   the last year. The purpose is to ensure that the trend plot begins and ends
#'   at the beginning or end of year.
#' @param k The smoothing value supplied to \code{gam} for the temporal and wind
#'   direction components, respectively. In some cases e.g. a trend plot with
#'   less than 1-year of data the smoothing with the default values may become
#'   too noisy and affected more by outliers. Choosing a lower value of \code{k}
#'   (say 10) may help produce a better plot.
#' @export
#' @return As well as generating the plot itself, \code{polarAnnulus} also
#'   returns an object of class ``openair''. The object includes three main
#'   components: \code{call}, the command used to generate the plot;
#'   \code{data}, the data frame of summarised information used to make the
#'   plot; and \code{plot}, the plot itself. If retained, e.g. using
#'   \code{output <- polarAnnulus(mydata, "nox")}, this output can be used to
#'   recover the data, reproduce or rework the original plot or undertake
#'   further analysis.
#'
#'   An openair output can be manipulated using a number of generic operations,
#'   including \code{print}, \code{plot} and \code{summary}.
#' @family polar directional analysis functions

polar_annulus <- function(data,
                          pollutant,
                          local_tz = NULL,
                          period = "hour",
                          facet = NULL,
                          statistic = "mean",
                          percentile = NA,
                          width = 1,
                          min_bin = 1,
                          exclude_missing = TRUE,
                          pad_date = FALSE,
                          force_positive = TRUE,
                          k = c(20, 10),
                          normalise = FALSE,
                          alpha = 1) {
  # respond to period
  scale <- switch(period,
    hour = c(0, 23),
    season = c(0, 12),
    weekday = c(0, 7),
    trend = c(0, 10)
  )

  breaks <- switch(period,
    hour = c(23, seq(0, 24, 6)),
    season = scale,
    weekday = scale,
    trend = c()
  )

  labels <- switch(period,
    hour = breaks,
    season = c("Jan", "Dec"),
    weekday = c("Sun", "Sat"),
    trend = c()
  )

  # run openair
  if (is.null(facet)) facet <- "default"
  oa_data <-
    openair::polarAnnulus(
      mydata = data,
      pollutant = pollutant,
      local.tz = local_tz,
      period = period,
      type = facet,
      statistic = statistic,
      percentile = percentile,
      min.bin = min_bin,
      exclude.missing = exclude_missing,
      date.pad = pad_date,
      force.positive = force_positive,
      k = k,
      normalise = normalise,
      plot = FALSE
    )$data

  # process data
  plot_data <-
    oa_data %>%
    tidyr::drop_na("z", "u", "v") %>%
    dplyr::mutate(
      r = sqrt(.data$u^2 + (.data$v * -1)^2),
      t = dplyr::if_else(.data$u < 0,
        atan((.data$v * -1) / .data$u) + pi,
        atan((.data$v * -1) / .data$u)
      ),
      t = (.data$t * (180 / pi)) + 90
    ) %>%
    dplyr::arrange("z")

  # sort out y axis
  plot_data <-
    dplyr::mutate(plot_data, r = scales::rescale(x = .data$r, to = scale))

  # sort out point size
  ps <- 1
  if (alpha < 1) {
    ps <- 0
  }

  # construct plot
  plt <-
    ggplot2::ggplot(plot_data, ggplot2::aes(.data$t, .data$r)) +
    ggplot2::coord_polar() +
    scattermore::geom_scattermore(
      interpolate = TRUE,
      pointsize = ps,
      ggplot2::aes(color = .data$z),
      na.rm = TRUE,
      alpha = alpha
    ) +
    ggplot2::expand_limits(y = -max(plot_data$r) * (1 / width)) +
    ggplot2::scale_x_continuous(
      breaks = seq(0, 270, 90),
      limits = c(0, 360),
      labels = c("N", "E", "S", "W")
    ) +
    ggplot2::labs(
      x = NULL, y = NULL,
      color = openair::quickText(paste(pollutant, collapse = ", "))
    ) +
    ggplot2::scale_y_continuous(breaks = breaks, labels = labels)

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
