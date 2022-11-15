#' Traditional wind rose plot and pollution rose variation
#'
#' @param data A data frame containing fields \code{ws} and
#'   \code{wd}
#' @param ws Name of the column representing wind speed.
#' @param wd Name of the column representing wind direction.
#' @param ws2 The user can supply a second set of wind speed and wind
#'   direction values with which the first can be compared. See
#'   details below for full explanation.
#' @param wd2 see \code{ws2}.
#' @param ws_int The Wind speed interval. Default is 2 m/s but for
#'   low met masts with low mean wind speeds a value of 1 or 0.5 m/s
#'   may be better. Note, this argument is superseded in
#'   \code{pollutionRose}. See \code{breaks} below.
#' @param angle Default angle of \dQuote{spokes} is 30. Other
#'   potentially useful angles are 45 and 10. Note that the width of
#'   the wind speed interval may need adjusting using \code{width}.
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
#' @param bias_corr When \code{angle} does not divide exactly into
#'   360 a bias is introduced in the frequencies when the wind
#'   direction is already supplied rounded to the nearest 10 degrees,
#'   as is often the case. For example, if \code{angle = 22.5}, N, E,
#'   S, W will include 3 wind sectors and all other angles will be
#'   two. A bias correction can made to correct for this problem. A
#'   simple method according to Applequist (2012) is used to adjust
#'   the frequencies.
#' @param breaks Most commonly, the number of break points for wind
#'   speed in \code{windRose} or pollutant in \code{pollutionRose}.
#'   For \code{windRose} and the \code{ws.int} default of 2 m/s, the
#'   default, 4, generates the break points 2, 4, 6, 8 m/s. For
#'   \code{pollutionRose}, the default, 6, attempts to breaks the
#'   supplied data at approximately 6 sensible break points. However,
#'   \code{breaks} can also be used to set specific break points. For
#'   example, the argument \code{breaks = c(0, 1, 10, 100)} breaks
#'   the data into segments <1, 1-10, 10-100, >100.
#' @param normalise If \code{TRUE} each wind direction segment of a
#'   pollution rose is normalised to equal one. This is useful for
#'   showing how the concentrations (or other parameters) contribute
#'   to each wind sector when the proprtion of time the wind is from
#'   that direction is low. A line showing the probability that the
#'   wind directions is from a particular wind sector is also shown.
#' @param dig.lab The number of signficant figures at which
#'   scientific number formatting is used in break point and key
#'   labelling. Default 5.
#' @param include.lowest Logical. If \code{FALSE} (the default), the first wind
#'   speed interval will be left exclusive and right inclusive. If \code{TRUE},
#'   the first interval will be left and right inclusive. Passed to the
#'   \code{include.lowest} argument of \code{cut()}.
#' @param statistic The \code{statistic} to be applied to each data
#'   bin in the plot. Options currently include \dQuote{prop.count},
#'   \dQuote{prop.mean} and \dQuote{abs.count}. The default
#'   \dQuote{prop.count} sizes bins according to the proportion of
#'   the frequency of measurements.  Similarly, \dQuote{prop.mean}
#'   sizes bins according to their relative contribution to the mean.
#'   \dQuote{abs.count} provides the absolute count of measurements
#'   in each bin.
#' @param pollutant Alternative data series to be sampled instead of
#'   wind speed. The \code{windRose} default NULL is equivalent to
#'   \code{pollutant = "ws"}.
#' @param border Border colour for shaded areas. Default is no
#'   border.

gg_polar_windrose <- function(data, type = "default") {
  oa_data <-
    openair::windRose(data, type = type, plot = F)$data

  data_long <-
    oa_data %>%
    dplyr::filter(wd >= 0) %>%
    tidyr::pivot_longer(dplyr::contains(" to ")) %>%
    dplyr::mutate(name = forcats::fct_inorder(name) |> forcats::fct_rev())

  axis_extend <-
    data_long %>%
    dplyr::pull(value) %>%
    range() %>%
    diff()
  axis_extend <- axis_extend / 4

  if (length(type) == 2) {
    data_grouped <-
      dplyr::group_by(data_long, .data[[type[1]]], .data[[type[2]]], .data$wd)
  } else {
    data_grouped <-
      dplyr::group_by(data_long, .data[[type[1]]], .data$wd)
  }

  plot_data <-
    data_grouped %>%
    dplyr::mutate(value = dplyr::if_else(!is.na(dplyr::lag(value)),
                                         value - dplyr::lag(value),
                                         value))  %>%
    dplyr::ungroup("wd") %>%
    dplyr::mutate(lab = stringr::str_glue("mean = {panel.fun}\ncalm = {calm}%"))

  plt <-
    ggplot2::ggplot(plot_data, ggplot2::aes(x = .data$wd)) +
    ggplot2::geom_col(
      ggplot2::aes(y = .data$value, fill = .data$name),
      width = 30 / (3 / 2),
      color = "white"
    ) +
    ggplot2::coord_polar(start = 15 / 360 * 2 * pi, clip = "off") +
    ggplot2::scale_x_continuous(
      breaks = c(90, 180, 270, 360),
      limits = c(15, 375),
      labels = c("E", "S", "W", "N")
    ) +
    ggopenair::theme_polar(guides = F, panel_ontop = F) +
    ggplot2::geom_label(
      size = 3,
      ggplot2::aes(
        x = 135,
        y = axis_extend*4,
        label = .data$lab
      )
    ) +
    ggplot2::labs(x = NULL, y = NULL, fill = NULL) +
    ggplot2::expand_limits(y = -axis_extend)

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
