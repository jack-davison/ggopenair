#' Annotate a Polar Plot with a Coloured Wedge
#'
#' This is a wrapper around [ggplot2::annotate()] which draws a semi-transparent
#' wedge on a plot with continuous polar coordinates (any of [gg_polar_plot()],
#' [gg_polar_annulus()], [gg_polar_freq()] or [gg_polar_percentile()]).
#'
#' @param start The angle at which to start the wedge. Can be expressed
#'   numerically, or using cardinal direction abbreviations ("N", "NNE", "NE",
#'   "ENE", etc.).
#' @param end The angle at which to end the wedge. See `end` for more details.
#' @param fill The fill colour of the wedge. Defaults to "red".
#' @param colour The border colour of the wedge. Defaults to `NA`, which means
#'   no border is drawn.
#' @param alpha The transparency of the wedge. `1` is totally opaque and `0` is
#'   completely transparent.
#' @param ... Arguments to pass to [ggplot2::annotate()].
#' @param .rose_angle The number passed to the `angle` argument of
#'   [gg_pollutionrose()]/[gg_windrose()]. This is required to adjust `start`
#'   and `end` when they are near to North/0 due to the way
#'   [ggplot2::geom_col()] interacts with [ggplot2::coord_polar()].
#'
#' @return An annotation to be added to [ggplot2::ggplot()].
#' @export
#' @family polar annotation functions
#' @examples
#' \dontrun{
#' gg_polar_plot(mydata, "nox") + annotate_polar_wedge(start = "N", end = "E")
#' }
#'
annotate_polar_wedge <-
  function(start,
           end,
           fill = "red",
           colour = NA,
           alpha = .25,
           ...,
           .rose_angle = NULL) {
    start <- str_to_angle(start)
    end <- str_to_angle(end)

    if (!is.null(.rose_angle)) {
      ra = .rose_angle / 2
      if (start >= 0 & start < ra) {
        start <- start + 360
      }
      if (end >= 0 & end < ra) {
        end <- end + 360
      }
      real0 = ra
      real360 = ra + 360
    } else {
      real0 = 0
      real360 = 360
    }

    annot_func <- function(start, end) {
      ggplot2::annotate(
        geom = "rect",
        xmin = start,
        xmax = end,
        ymin = 0,
        ymax = Inf,
        alpha = alpha,
        fill = fill,
        color = colour,
        ...
      )
    }

    if (end > start) {
      annot_func(start, end)
    } else {
      list(
        annot_func(start, real360),
        annot_func(real0, end)
      )
    }
  }

#' Annotate a Polar Plot with Axis Labels
#'
#' This is a wrapper around [ggplot2::annotate()] which draws the y-axis labels
#' directly on the plot, and optionally drops the labels at the side. Note that,
#' owing to the way [ggplot2::geom_col()] interacts with
#' [ggplot2::coord_polar()], the "direction" argument may require more
#' trial-and-error when used with [gg_polar_pollrose()] and
#' [gg_polar_windrose()].
#'
#' @param breaks The axis breaks to label on the plot. It may be appropriate to
#'   use [seq()] to obtain equally spaced labels.
#' @param direction The angles at which to write the labels. Can be expressed
#'   numerically, or using cardinal direction abbreviations ("N", "NNE", "NE",
#'   "ENE", etc.).
#' @param drop Remove the y-axis labels to the side of the plot? Defaults to
#'   `TRUE`. Note that this option is overriden by complete themes such as
#'   [ggplot2::theme_minimal()] or [theme_polar()].
#' @param ... Arguments to pass to [ggplot2::annotate()].
#' @param .rose_angle The number passed to the `angle` argument of
#'   [gg_pollutionrose()]/[gg_windrose()]. This is required to adjust `wd` when
#'   it is near to North/0 due to the way [ggplot2::geom_col()] interacts with
#'   [ggplot2::coord_polar()].
#' @return An annotation to be added to [ggplot2::ggplot()].
#' @export
#' @family polar annotation functions
#' @examples
#' \dontrun{
#' gg_polar_plot(mydata, "nox") + annotate_polar_axis(seq(5, 25, 5))
#' }
#'
annotate_polar_axis <- function(breaks, direction = "NW", drop = TRUE, ..., .rose_angle = NULL) {
  x <- str_to_angle(direction)

  if (!is.null(.rose_angle)) {
    ra = .rose_angle / 2
    if (x >= 0 & x < ra) {
      x <- x + 360
    }
  }

  func <- function(.x) {
    ggplot2::annotate(
      geom = "text",
      x = x,
      y = .x,
      label = as.character(.x),
      ...
    )
  }

  out <-
    purrr::map(
      .x = breaks,
      .f = func
    )

  if (drop) {
    out <-
      append(
        out,
        list(ggplot2::theme(
          axis.text.y = ggplot2::element_blank(),
          axis.ticks.y = ggplot2::element_blank()
        ))
      )
  }

  out
}

#' Turn cardinal direction to numeric
#' @param x A string.
#' @noRd
str_to_angle <- function(x) {
  if (is.character(x)) {
    dplyr::case_when(
      x == "N" ~ 0,
      x == "NNE" ~ 22.5,
      x == "NE" ~ 45,
      x == "ENE" ~ 67.5,
      x == "E" ~ 90,
      x == "ESE" ~ 112.5,
      x == "SE" ~ 135,
      x == "SSE" ~ 157.5,
      x == "S" ~ 180,
      x == "SSW" ~ 202.5,
      x == "SW" ~ 225,
      x == "WSW" ~ 247.5,
      x == "W" ~ 270,
      x == "WNW" ~ 292.5,
      x == "NW" ~ 315,
      x == "NNW" ~ 337.5,
      TRUE ~ NA_real_
    )
  } else {
    x
  }
}
