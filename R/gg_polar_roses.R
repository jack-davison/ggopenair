#' Traditional wind rose plot and pollution rose variation
#'
#' @inheritParams gg_polar_plot
#' @param pollutant A column name identifying a pollutant concentration.
#' @param angle Angle of the spokes. Ideally a number by which 360 is evenly
#'   divisible (see \code{bias_corr}).
#' @param bias_corr When \code{angle} does not divide exactly into 360 a bias is
#'   introduced in the frequencies when the wind direction is already supplied
#'   rounded to the nearest 10 degrees, as is often the case. For example, if
#'   \code{angle = 22.5}, N, E, S, W will include 3 wind sectors and all other
#'   angles will be two. A bias correction can made to correct for this problem.
#'   A simple method according to Applequist (2012) is used to adjust the
#'   frequencies.
#' @param breaks The number of break points to use when binning the data, or a
#'   vector of specific break points. For example, \code{breaks = 6} will split
#'   the legend into roughly 6 equal bins, whereas \code{breaks = c(0, 1, 10,
#'   100)} breaks the data into four segments: <1, 1-10, 10-100, & >100.
#' @param normalise If \code{TRUE} each wind direction segment is normalised to
#'   equal one. This is useful for showing how the concentrations (or other
#'   parameters) contribute to each wind sector when the proprtion of time the
#'   wind is from that direction is low. A line showing the probability that the
#'   wind directions is from a particular wind sector is also shown.
#' @param statistic The \code{statistic} to be applied to each data bin in the
#'   plot. Options currently include \dQuote{prop.count}, \dQuote{prop.mean} and
#'   \dQuote{abs.count}. The default \dQuote{prop.count} sizes bins according to
#'   the proportion of the frequency of measurements.  Similarly,
#'   \dQuote{prop.mean} sizes bins according to their relative contribution to
#'   the mean. \dQuote{abs.count} provides the absolute count of measurements in
#'   each bin.
#' @param width Width of each bar as a fraction of its maximum width.
#'   \code{width = 1} makes all bars meet at their edges and \code{width = 0}
#'   makes them disappear entirely. Defaults to \code{0.9}.
#' @param border_colour Border colour for shaded areas. Default is no border.
#' @export
#' @family polar directional analysis plotting functions
#' @seealso [annotate_rose_text()] for adding an annotation of mean and calm
#'   conditions to the figure.
gg_pollutionrose <-
  function(data,
           pollutant,
           angle = 30,
           facet = NULL,
           bias_corr = TRUE,
           breaks = 6,
           normalise = FALSE,
           statistic = "prop.count",
           width = 0.9,
           border_colour = NA,
           alpha = 1) {
    # run openair
    if (is.null(facet)) facet <- "default"
    oa_data <-
      openair::pollutionRose(
        mydata = data,
        pollutant = pollutant,
        angle = angle,
        type = facet,
        bias.corr = bias_corr,
        breaks = breaks,
        plot = FALSE,
        normalise = normalise,
        statistic = statistic
      )$data

    if (width > 1) {
      width <- 1
    }

    data_long <-
      oa_data %>%
      dplyr::filter(.data$wd >= 0) %>%
      tidyr::pivot_longer(dplyr::contains(" to ")) %>%
      dplyr::mutate(name = forcats::fct_inorder(.data$name) %>%
                      forcats::fct_rev())

    axis_extend <-
      data_long %>%
      dplyr::pull(.data$value) %>%
      pretty() %>%
      diff() %>%
      unique()
    axis_extend <- axis_extend * 3 / 4

    if (length(facet) == 2) {
      data_grouped <-
        dplyr::group_by(data_long, .data[[facet[1]]], .data[[facet[2]]], .data$wd)
    } else {
      data_grouped <-
        dplyr::group_by(data_long, .data[[facet[1]]], .data$wd)
    }

    plot_data <-
      data_grouped %>%
      dplyr::mutate(value = dplyr::if_else(!is.na(dplyr::lag(.data$value)),
                                           .data$value - dplyr::lag(.data$value),
                                           .data$value
      )) %>%
      dplyr::ungroup("wd") %>%
      dplyr::mutate(
        lab = stringr::str_glue("mean = {panel.fun}\ncalm = {calm}%")
      )

    plt <-
      ggplot2::ggplot(plot_data, ggplot2::aes(x = .data$wd)) +
      ggplot2::geom_col(
        ggplot2::aes(y = .data$value, fill = .data$name),
        width = angle * width,
        color = border_colour,
        alpha = alpha
      ) +
      ggplot2::coord_polar(start = (angle / 2) / 360 * 2 * pi, clip = "off") +
      ggplot2::scale_x_continuous(
        breaks = c(90, 180, 270, 360),
        limits = c((angle / 2), 360 + (angle / 2)),
        labels = c("E", "S", "W", "N")
      ) +
      ggplot2::labs(x = NULL, y = NULL, fill = openair::quickText(pollutant)) +
      ggplot2::expand_limits(y = -axis_extend)

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

#' Traditional wind rose plot and pollution rose variation
#'
#' @inheritParams gg_pollutionrose
#' @param data A data frame containing fields \code{ws} and \code{wd}
#' @param ws Name of the column representing wind speed.
#' @param wd Name of the column representing wind direction.
#' @param ws_int The Wind speed interval. Default is 2 m/s but for low met masts
#'   with low mean wind speeds a value of 1 or 0.5 m/s may be better. Note, this
#'   argument is superseded in \code{pollutionRose}. See \code{breaks} below.
#' @param breaks The number of break points to use when binning the data (used
#'   alongside \code{ws_int}), or a vector of specific break points. For
#'   example, \code{breaks = 6} and \code{ws_int = 2} generates the break points
#'   2, 4, 6 & 8 m/s, whereas \code{breaks = c(0, 1, 10, 100)} breaks the data
#'   into four segments: <1, 1-10, 10-100, & >100.
#' @export
#' @family polar directional analysis plotting functions
gg_windrose <-
  function(data,
           ws = "ws",
           wd = "wd",
           ws_int = 2,
           angle = 30,
           facet = NULL,
           bias_corr = TRUE,
           breaks = 4,
           normalise = FALSE,
           statistic = "prop.count",
           width = 0.9,
           border_colour = NA,
           alpha = 1) {
    # run openair
    if (is.null(facet)) facet <- "default"
    oa_data <-
      openair::windRose(
        data,
        ws = ws,
        wd = wd,
        ws2 = ws2,
        wd2 = wd2,
        ws.int = ws_int,
        angle = angle,
        type = facet,
        bias.corr = bias_corr,
        breaks = breaks,
        plot = FALSE,
        normalise = normalise,
        statistic = statistic
      )$data

    if (width > 1) {
      width <- 1
    }

    data_long <-
      oa_data %>%
      dplyr::filter(wd >= 0) %>%
      tidyr::pivot_longer(dplyr::contains(" to ")) %>%
      dplyr::mutate(name = forcats::fct_inorder(.data$name) |> forcats::fct_rev())

    axis_extend <-
      data_long %>%
      dplyr::pull(.data$value) %>%
      pretty() %>%
      diff() %>%
      unique()
    axis_extend <- axis_extend * 3 / 4

    if (length(facet) == 2) {
      data_grouped <-
        dplyr::group_by(data_long, .data[[facet[1]]], .data[[facet[2]]], .data$wd)
    } else {
      data_grouped <-
        dplyr::group_by(data_long, .data[[facet[1]]], .data$wd)
    }

    plot_data <-
      data_grouped %>%
      dplyr::mutate(value = dplyr::if_else(
        !is.na(dplyr::lag(.data$value)),
        .data$value - dplyr::lag(.data$value),
        .data$value
      )) %>%
      dplyr::ungroup("wd") %>%
      dplyr::mutate(lab = stringr::str_glue("mean = {panel.fun}\ncalm = {calm}%"))

    plt <-
      ggplot2::ggplot(plot_data, ggplot2::aes(x = .data$wd)) +
      ggplot2::geom_col(
        alpha = alpha,
        ggplot2::aes(y = .data$value, fill = .data$name),
        width = angle * width,
        color = border_colour
      ) +
      ggplot2::coord_polar(start = (angle / 2) / 360 * 2 * pi, clip = "off") +
      ggplot2::scale_x_continuous(
        breaks = c(90, 180, 270, 360),
        limits = c((angle / 2), 360 + (angle / 2)),
        labels = c("E", "S", "W", "N")
      ) +
      ggplot2::labs(x = NULL, y = NULL, fill = openair::quickText("ws")) +
      ggplot2::expand_limits(y = -axis_extend)

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

#' Annotate a wind or pollution rose with further information
#'
#' Add an annotation to a [gg_pollutionrose()] or [gg_windrose()] plot. This
#' annotation typically shows the mean pollutant or wind speed value, along with
#' the percentage calm value.
#'
#' @param y Y value at which to draw the annotation.
#' @param wd The wind direction at which to draw the annotation. Can be either
#'   numeric, where 0 is North and 180 is South, or a character representing a
#'   cardinal direction ("N", "NE", "E", etc.).
#' @param rose_angle The number passed to the `angle` argument of
#'   [gg_pollutionrose()]/[gg_windrose()]. This is required to adjust `wd` due
#'   to the way [ggplot2::geom_col()] interacts with [ggplot2::coord_polar()].
#' @param size Size of the annotation.
#' @param alpha Alpha value of the annotation, where 1 is opaque and 0 is
#'   transparent.
#' @param fun One of "label" or "text", which will use [ggplot2::geom_label()]
#'   or [ggplot2::geom_text()], respectively.
#' @param ... Other arguments to pass to `fun`.
#' @export
#' @family polar annotation functions
annotate_rose_text <-
  function(y,
           wd = 135,
           rose_angle = 30,
           size = 3,
           alpha = 1,
           fun = "label",
           ...) {

    fun = match.arg(fun, c("label", "text"))

    if (is.character(wd)) {
      wd <- str_to_angle(wd)
    }

    x = rose_angle / 2
    if (wd >= 0 & wd < x) {
      wd <- wd + 360
    }
    wd2 <- wd

    if (fun == "label") {
      ggplot2::geom_label(...,
                          data = . %>% dplyr::slice_head(n = 1),
                          size = size,
                          alpha = alpha,
                          ggplot2::aes(x = wd2,
                                       y = y,
                                       label = .data$lab))
    } else if (fun == "text") {
      ggplot2::geom_text(
        ...,
        check_overlap = TRUE,
        size = size,
        alpha = alpha,
        ggplot2::aes(x = wd2,
                     data = . %>% dplyr::slice_head(n = 1),
                     y = y,
                     label = .data$lab)
      )
    }


  }
