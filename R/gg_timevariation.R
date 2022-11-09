#' Diurnal, day of the week and monthly variation
#'
#' Plots the diurnal, day of the week and monthly variation for different
#' variables, typically pollutant concentrations. Four separate plots are
#' produced.
#'
#' The variation of pollutant concentrations by hour of the day and day of the
#' week, etc., can reveal many interesting features that relate to source types
#' and meteorology. For traffic sources, there are often important differences
#' in the way vehicles vary by vehicles type, e.g., less heavy vehicles at
#' weekends.
#'
#' The plots also show the 95\% confidence intervals in the mean. The 95\%
#' confidence intervals in the mean are calculated through bootstrap
#' simulations, which will provide more robust estimates of the confidence
#' intervals (particularly when there are relatively few data).
#'
#' The function can handle multiple pollutants and uses the flexible \code{type}
#' option to provide separate panels for each 'type' --- see \code{cutData} for
#' more details. It can also accept a \code{group} option which is useful if
#' data are stacked. This will work in a similar way to having multiple
#' pollutants in separate columns.
#'
#' The option \code{difference} will calculate the difference in means of two
#' pollutants together with bootstrap estimates of the 95\% confidence
#' intervals in the difference in the mean. This works in two ways: either two
#' pollutants are supplied in separate columns, e.g., `pollutant = c("no2", "o3")`
#' or there are two unique values of \code{group}. The difference is
#' calculated as the second pollutant minus the first and is labelled as such.
#' Considering differences in this way can provide many useful insights and is
#' particularly useful for model evaluation when information is needed about
#' where a model differs from observations by many different time scales. The
#' manual contains various examples of using \code{difference = TRUE}.
#'
#' Note also that the \code{timeVariation} function works well on a subset of
#' data and in conjunction with other plots. For example, a
#' \code{\link{polarPlot}} may highlight an interesting feature for a
#' particular wind speed/direction range. By filtering for those conditions
#' \code{timeVariation} can help determine whether the temporal variation of
#' that feature differs from other features --- and help with source
#' identification.
#'
#' In addition, \code{timeVariation} will work well with other variables if
#' available. Examples include meteorological and traffic flow data.
#'
#' Depending on the choice of statistic, a subheading is added. Users can
#' control the text in the subheading through the use of \code{sub} e.g.
#' \code{sub = ""} will remove any subheading.
#'
#' @param mydata A data frame of hourly (or higher temporal resolution data).
#'   Must include a \code{date} field and at least one variable to plot.
#' @param pollutant Name of variable to plot. Two or more pollutants can be
#'   plotted, in which case a form like \code{pollutant = c("nox", "co")}
#'   should be used.
#' @param local_tz Should the results be calculated in local time that includes
#'   a treatment of daylight savings time (DST)? The default is not to consider
#'   DST issues, provided the data were imported without a DST offset.
#'   Emissions activity tends to occur at local time e.g. rush hour is at 8 am
#'   every day. When the clocks go forward in spring, the emissions are
#'   effectively released into the atmosphere typically 1 hour earlier during
#'   the summertime i.e. when DST applies. When plotting diurnal profiles, this
#'   has the effect of \dQuote{smearing-out} the concentrations. Sometimes, a
#'   useful approach is to express time as local time. This correction tends to
#'   produce better-defined diurnal profiles of concentration (or other
#'   variables) and allows a better comparison to be made with
#'   emissions/activity data. If set to \code{FALSE} then GMT is used. Examples
#'   of usage include \code{local.tz = "Europe/London"}, \code{local.tz =
#'   "America/New_York"}. See \code{cutData} and \code{import} for more
#'   details.
#' @param normalise Should variables be normalised? The default is
#'   \code{FALSE}. If \code{TRUE} then the variable(s) are divided by their
#'   mean values. This helps to compare the shape of the diurnal trends for
#'   variables on very different scales.
#' @param type \code{type} determines how the data are split i.e. conditioned,
#'   and then plotted. The default is will produce a single plot using the
#'   entire data. Type can be one of the built-in types as detailed in
#'   \code{cutData} e.g. \dQuote{season}, \dQuote{year}, \dQuote{weekday} and
#'   so on. For example, \code{type = "season"} will produce four plots --- one
#'   for each season.
#'
#'   It is also possible to choose \code{type} as another variable in the data
#'   frame. If that variable is numeric, then the data will be split into four
#'   quantiles (if possible) and labelled accordingly. If type is an existing
#'   character or factor variable, then those categories/levels will be used
#'   directly. This offers great flexibility for understanding the variation of
#'   different variables and how they depend on one another.
#'
#'   Only one \code{type} is allowed in\code{timeVariation}.
#' @param group This sets the grouping variable to be used. For example, if a
#'   data frame had a column \code{site} setting \code{group = "site"} will
#'   plot all sites together in each panel. See examples below.
#' @param difference If two pollutants are chosen then setting \code{difference
#'   = TRUE} will also plot the difference in means between the two variables
#'   as \code{pollutant[2] - pollutant[1]}. Bootstrap 95\% confidence intervals
#'   of the difference in means are also calculated. A horizontal dashed line
#'   is shown at y = 0. The difference can also be calculated if there is a
#'   column that identifies two groups e.g. having used \code{splitByDate}. In
#'   this case it is possible to call \code{timeVariation} with the option
#'   \code{group = "split.by"} and \code{difference = TRUE}.
#' @param statistic Can be \dQuote{mean} (default) or \dQuote{median}. If the
#'   statistic is \sQuote{mean} then the mean line and the 95\% confidence
#'   interval in the mean are plotted by default. If the statistic is
#'   \sQuote{median} then the median line is plotted together with the 5/95 and
#'   25/75th quantiles are plotted. Users can control the confidence intervals
#'   with \code{conf.int}.
#' @param conf_int The confidence intervals to be plotted. If \code{statistic =
#'   "mean"} then the confidence intervals in the mean are plotted. If
#'   \code{statistic = "median"} then the \code{conf.int} and \code{1 -
#'   conf.int} \emph{quantiles} are plotted. \code{conf.int} can be of length
#'   2, which is most useful for showing quantiles. For example \code{conf.int
#'   = c(0.75, 0.99)} will yield a plot showing the median, 25/75 and 5/95th
#'   quantiles.
#' @param b Number of bootstrap replicates to use. Can be useful to reduce this
#'   value when there are a large number of observations available to increase
#'   the speed of the calculations without affecting the 95\% confidence
#'   interval calculations by much.
#' @param ci Should confidence intervals be shown? The default is \code{TRUE}.
#'   Setting this to \code{FALSE} can be useful if multiple pollutants are
#'   chosen where over-lapping confidence intervals can over complicate plots.
#' @param alpha The alpha transparency used for plotting confidence intervals.
#'   0 is fully transparent and 1 is opaque. The default is 0.4
#' @param return What should the function return? One of:
#' * "ensemble" --- all four time variation panels assembled as a 'patchwork' object (default).
#' * "day_hour", "day", "hour", "month" --- a single time variation panel.
#' * "list" --- a list of the four time variation panels, which may be useful if
#' users wish to assemble them in a different way or with other plots entirely.
#' * "data" --- the raw data used to create the time variation panels.
#' @export
#'
gg_timevariation <- function(mydata,
                             pollutant = "nox",
                             local_tz = NULL,
                             normalise = FALSE,
                             type = "default",
                             group = NULL,
                             difference = FALSE,
                             statistic = "mean",
                             conf_int = 0.95,
                             b = 100,
                             ci = TRUE,
                             alpha = 0.3,
                             return = "ensemble") {
  # get ylab
  if (normalise) {
    ylab <-
      "normalised level"
  } else {
    ylab <- openair::quickText(paste(pollutant, collapse = ", "))
  }

  # run openair
  if (!is.null(group)) {
    oa_data <-
      openair::timeVariation(
        mydata = mydata,
        pollutant = pollutant,
        local.tz = local_tz,
        normalise = normalise,
        type = type,
        group = group,
        difference = difference,
        statistic = statistic,
        conf.int = conf_int,
        B = b,
        ci = ci,
        alpha = alpha,
        plot = FALSE
      )$data
  } else {
    oa_data <-
      openair::timeVariation(
        mydata = mydata,
        pollutant = pollutant,
        local.tz = local_tz,
        normalise = normalise,
        type = type,
        difference = difference,
        statistic = statistic,
        conf.int = conf_int,
        B = b,
        ci = ci,
        alpha = alpha,
        plot = FALSE
      )$data
  }

  oa_data <- purrr::map(oa_data[1:4], dplyr::ungroup)

  drop_legend <- return != "ensemble"

  # return data if that's all that is wanted
  out_data <- list(
    hour = oa_data$hour,
    day = oa_data$day,
    day_hour = oa_data$day.hour,
    month = oa_data$month
  )
  if (return == "data") {
    return(out_data)
  }

  # hour plot
  plt_hour <- tv_panel_line(oa_data$hour, ci_check = ci, drop_legend = drop_legend, alpha = alpha) +
    ggplot2::labs(y = ylab)

  if (type != "default") {
    plt_hour <- plt_hour +
      ggplot2::facet_wrap(ggplot2::vars(.data[[type]]))
  }

  if (return == "hour") {
    return(plt_hour)
  }

  # day hour plot
  plt_day_hour <- tv_panel_line(oa_data$day.hour, ci_check = ci, drop_legend = T, alpha = alpha) +
    ggplot2::labs(y = ylab)

  if (type != "default") {
    plt_day_hour <- plt_day_hour +
      ggplot2::facet_grid(
        rows = ggplot2::vars(.data[[type]]),
        cols = ggplot2::vars(.data$wkday)
      )
  } else {
    plt_day_hour <- plt_day_hour +
      ggplot2::facet_grid(cols = ggplot2::vars(.data$wkday))
  }

  if (return == "day_hour") {
    return(plt_day_hour)
  }

  # day plot
  plt_day <-
    tv_panel_crossbar(oa_data$day, "wkday", ci_check = ci, type = type, drop_legend = drop_legend, alpha = alpha) +
    ggplot2::scale_x_continuous(
      breaks = 1:7,
      labels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"),
      name = "weekday"
    ) +
    ggplot2::labs(y = ylab)

  if (return == "day") {
    return(plt_day)
  }

  # month plot
  plt_month <-
    tv_panel_crossbar(oa_data$month, "mnth", ci_check = ci, type = type, drop_legend = drop_legend, alpha = alpha) +
    ggplot2::scale_x_continuous(
      breaks = 1:12,
      labels = c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D"),
      name = "month"
    ) +
    ggplot2::labs(y = ylab)

  if (return == "month") {
    return(plt_month)
  }

  if (return == "list") {
    out <- list(
      hour = plt_hour,
      day = plt_day,
      day_hour = plt_day_hour,
      month = plt_month
    )
    return(out)
  }

  # ensemble output
  if (return == "ensemble") {
    bottom <- patchwork::wrap_plots(plt_hour, plt_day, plt_month)
    (patchwork::wrap_plots(plt_day_hour, bottom, ncol = 1) +
        patchwork::plot_layout(guides = "collect")) &
      ggplot2::labs(y = ylab)
  }
}

#' make panel for categorical axes (month/weekday)
#' @param data data
#' @param ci_check show ci?
#' @param type type (from parent func)
#' @param drop_legend from parent func
#' @param alpha alpha
#' @noRd
tv_panel_crossbar <- function(data, x, ci_check, type, drop_legend, alpha) {
  plt <-
    data %>%
    ggplot2::ggplot(ggplot2::aes(
      x = .data[[x]],
      y = .data$Mean,
      ymax = .data$Upper,
      ymin = .data$Lower
    )) +
    ggplot2::geom_line(
      show.legend = drop_legend,
      ggplot2::aes(color = .data$variable)
    ) +
    ggplot2::geom_crossbar(
      show.legend = drop_legend,
      width = .5,
      ggplot2::aes(
        ymax = .data$Mean,
        ymin = .data$Mean,
        color = .data$variable
      )
    )

  if (ci_check) {
    plt <-
      plt +
      ggplot2::geom_crossbar(
        show.legend = drop_legend,
        width = .5,
        color = NA,
        ggplot2::aes(
          group = interaction(.data$ci, .data$variable),
          fill = .data$variable
        ),
        alpha = alpha
      )
  }

  if (type != "default") {
    plt <- plt +
      ggplot2::facet_wrap(ggplot2::vars(.data[[type]]))
  }

  plt
}

#' make panel for hour axes (hour/dayhour)
#' @param data data
#' @param ci show ci?
#' @param drop_legend from parent func
#' @param alpha alpha
#' @noRd
tv_panel_line <- function(data, ci_check, drop_legend, alpha) {
  plt <-
    data %>%
    tidyr::complete(.data$variable, .data$hour) %>%
    ggplot2::ggplot(ggplot2::aes(
      x = .data$hour,
      y = .data$Mean,
      ymax = .data$Upper,
      ymin = .data$Lower
    )) +
    ggplot2::geom_line(
      show.legend = drop_legend,
      ggplot2::aes(color = .data$variable),
      na.rm = TRUE
    ) +
    ggplot2::scale_x_continuous(
      expand = ggplot2::expansion(),
      breaks = seq(0, 24, 4)
    )

  if (ci_check) {
    plt <-
      plt + ggplot2::geom_ribbon(
        show.legend = drop_legend,
        ggplot2::aes(
          group = interaction(.data$ci, .data$variable),
          fill = .data$variable
        ),
        alpha = alpha
      )
  }
  plt
}
