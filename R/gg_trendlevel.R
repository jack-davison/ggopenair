#' Trend Heat Map
#'
#' The trendLevel function provides a way of rapidly showing a large amount of
#' data in a condensed form. In one plot, the variation in the concentration of
#' one pollutant can to shown as a function of up to three other categorical
#' properties. The default version of the plot uses y = hour of day, x = month
#' of year and type = year to provide information on trends, seasonal effects
#' and diurnal variations. However, x, y and type and summarising statistics can
#' all be modified to provide a range of other similar plots.
#'
#' \code{trendLevel} allows the use of third party summarising functions via the
#' \code{statistic} option. Any additional function arguments not included
#' within a function called using \code{statistic} should be supplied as a list
#' of named parameters and sent using \code{stat.args}. For example, the encoded
#' option \code{statistic = "mean"} is equivalent to \code{statistic = mean,
#' stat.args = list(na.rm = TRUE)} or the R command \code{mean(x, na.rm= TRUE)}.
#' Many R functions and user's own code could be applied in a similar fashion,
#' subject to the following restrictions: the first argument sent to the
#' function must be the data series to be analysed; the name `x' cannot be used
#' for any of the extra options supplied in \code{stat.args}; and the function
#' should return the required answer as a numeric or \code{NA}. Note: If the
#' supplied function returns more than one answer, currently only the first of
#' these is retained and used by \code{trendLevel}. All other returned
#' information will be ignored without warning. If the function terminates with
#' an error when it is sent an empty data series, the option
#' \code{stat.safe.mode} should not be set to \code{FALSE} or \code{trendLevel}
#' may fail. Note: The \code{stat.safe.mode = TRUE} option returns an NA without
#' warning for empty data series.
#'
#' @param data The openair data frame to use to generate the \code{trendLevel}
#'   plot.
#' @param pollutant A column name identifying a pollutant concentration.
#' @param x,y,facet The name of the data series with which to bin \code{mydata}.
#'   `x` and `y` form the x and y axes of the resulting plot, and `facet` is
#'   optionally used to separate the plot into separate panels. Duplication in
#'   \code{x}, \code{y} and \code{facet} are not permitted. If not
#'   representative of column names, the three arguments are passed to
#'   [openair::cutData()].
#' @param statistic The statistic method to be use to summarise locally binned
#'   \code{pollutant} measurements with. Three options are currently encoded:
#'   \dQuote{mean} (default), \dQuote{max} and \dQuote{frequency}. (Note:
#'   Functions can also be sent directly via \code{statistic}.  However, this
#'   option is still in development and should be used with caution. See Details
#'   below.)
#' @param stat_args Additional options to be used with \code{statistic} if this
#'   is a function. The extra options should be supplied as a list of named
#'   parameters. (see Details below.)
#' @param stat_safemode An addition protection applied when using functions
#'   direclty with \code{statistic} that most users can ignore. This option
#'   returns \code{NA} instead of running \code{statistic} on binned subsamples
#'   that are empty. Many common functions terminate with an error message when
#'   applied to an empty dataset. So, this option provides a mechanism to work
#'   with such functions. For a very few cases, e.g. for a function that counted
#'   missing entries, it might need to be set to \code{FALSE} (see Details
#'   below.)
#' @export
#' @return A [ggplot2::ggplot2] figure

gg_trendlevel <-
  function(data,
           pollutant,
           x,
           y,
           facet = NULL,
           statistic = c("mean", "max", "frequency"),
           stat_args = NULL,
           stat_safemode = TRUE) {
    if (is.null(facet)) facet <- "default"
    oa_data <-
      openair::trendLevel(
        mydata = data,
        pollutant = pollutant,
        x = x,
        y = y,
        type = facet,
        statistic = statistic,
        stat.args = stat_args,
        stat.safe.mode = stat_safemode,
        plot = FALSE
      )$data

    oa_names <- names(oa_data)

    plt <-
      ggplot2::ggplot(oa_data, ggplot2::aes(x = .data[[oa_names[[1]]]], y = .data[[oa_names[[2]]]])) +
      ggplot2::geom_tile(ggplot2::aes(fill = .data[[oa_names[[4]]]])) +
      ggplot2::coord_cartesian(expand = FALSE)

    if (oa_names[3] != "default") {
      plt <- plt +
        ggplot2::facet_wrap(ggplot2::vars(.data[[oa_names[[3]]]]))
    }

    return(plt)
  }
