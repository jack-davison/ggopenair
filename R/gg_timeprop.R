#' Time series plot with categories shown as a stacked bar chart
#'
#' This function shows time series plots as stacked bar charts. The
#' different categories in the bar chart are made up from a character
#' or factor variable in a data frame. The function is primarily
#' developed to support the plotting of cluster analysis output from
#' \code{\link{polarCluster}} and \code{\link{trajCluster}} that
#' consider local and regional (back trajectory) cluster analysis
#' respectively. However, the function has more general use for
#' understanding time series data.
#'
#' In order to plot time series in this way, some sort of time
#' aggregation is needed, which is controlled by the option
#' \code{avg_time}.
#'
#' The plot shows the value of \code{pollutant} on the y-axis
#' (averaged according to \code{avg_time}). The time intervals are
#' made up of bars split according to \code{proportion}. The bars
#' therefore show how the total value of \code{pollutant} is made up
#' for any time interval.
#'
#' @param data A data frame containing the fields \code{date},
#'   \code{pollutant} and a splitting variable \code{proportion}
#' @param pollutant Name of the pollutant to plot contained in
#'   \code{mydata}.
#' @param proportion The splitting variable that makes up the bars in
#'   the bar chart e.g. \code{proportion = "cluster"} if the output
#'   from \code{polarCluster} is being analysed. If \code{proportion}
#'   is a numeric variable it is split into 4 quantiles (by default)
#'   by \code{cutData}. If \code{proportion} is a factor or character
#'   variable then the categories are used directly.
#' @param avg_time This defines the time period to average to. Can be
#'   \dQuote{sec}, \dQuote{min}, \dQuote{hour}, \dQuote{day},
#'   \dQuote{DSTday}, \dQuote{week}, \dQuote{month}, \dQuote{quarter}
#'   or \dQuote{year}. For much increased flexibility a number can
#'   precede these options followed by a space. For example, a
#'   timeAverage of 2 months would be \code{period = "2 month"}.
#'
#'   Note that \code{avg_time} when used in \code{timeProp} should be
#'   greater than the time gap in the original data. For example,
#'   \code{avg_time = "day"} for hourly data is OK, but
#'   \code{avg_time = "hour"} for daily data is not.
#' @param facet \code{facet} determines how the data are split i.e.
#'   conditioned, and then plotted. The default is will produce a
#'   single plot using the entire data. Type can be one of the
#'   built-in types as detailed in \code{cutData} e.g. "season",
#'   "year", "weekday" and so on. For example, \code{facet = "season"}
#'   will produce four plots --- one for each season.
#'
#'   It is also possible to choose \code{facet} as another variable in
#'   the data frame. If that variable is numeric, then the data will
#'   be split into four quantiles (if possible) and labelled
#'   accordingly. If facet is an existing character or factor
#'   variable, then those categories/levels will be used directly.
#'   This offers great flexibility for understanding the variation of
#'   different variables and how they depend on one another.
#'
#'   \code{facet} must be of length one.
#' @param normalise If \code{normalise = TRUE} then each time
#'   interval is scaled to 100. This is helpful to show the relative
#'   (percentage) contribution of the proportions.
#' @export
#' @return A [ggplot2::ggplot2] figure

gg_timeprop <-
  function(data,
           pollutant,
           proportion,
           avg_time = "day",
           facet = "default",
           normalise = FALSE) {
    # run openair
    oa_data <-
      openair::timeProp(
        mydata = data,
        pollutant = pollutant,
        proportion = "wd",
        avg.time = avg_time,
        type = facet,
        plot = FALSE,
        normalise = normalise
      )$data

    # create plot
    plt <-
      oa_data %>%
      tidyr::drop_na(.data$var2) %>%
      dplyr::mutate(
        fill = forcats::fct_rev(.data[[proportion]]),
        Var1 = dplyr::lag(.data$var2) %>% tidyr::replace_na(0)
      ) %>%
      ggplot2::ggplot(ggplot2::aes(
        xmin = .data$xleft,
        xmax = .data$xright,
        ymin = .data$Var1,
        ymax = .data$var2
      )) +
      ggplot2::geom_rect(ggplot2::aes(fill = .data$fill)) +
      ggplot2::coord_cartesian(expand = FALSE) +
      ggplot2::labs(
        x = "date", y = openair::quickText(pollutant),
        fill = openair::quickText(proportion)
      )

    # sort out facets
    facet <- dplyr::group_vars(oa_data)
    facet <- facet[!facet %in% c("xleft", "xright", "date")]
    if (facet != "default") {
      plt <-
        plt + ggplot2::facet_wrap(facets = ggplot2::vars(.data[[facet]]))
    }

    # return plot
    return(plt)
  }
