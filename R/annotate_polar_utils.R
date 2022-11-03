#' Annotate a Polar Plot with a Coloured Wedge
#'
#' This is a wrapper around [ggplot2::annotate()] which draws a semi-transparent
#' wedge on a [gg_polar()] plot.
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
#'
#' @return An annotation to be added to [ggplot2::ggplot()].
#' @export
#'
#' @examples
#' \dontrun{
#' gg_polar(mydata, "nox") + annotate_polar_wedge(start = "N", end = "E")
#' }
#'
annotate_polar_wedge <- function(start, end, fill = "red", colour = NA, alpha = .25, ...) {
  if (is.character(start)) start <- str_to_angle(start)
  if (is.character(end)) end <- str_to_angle(end)

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
      annot_func(start, 360),
      annot_func(0, end)
    )
  }
}

#' Annotate a Polar Plot with Axis Labels
#'
#' This is a wrapper around [ggplot2::annotate()] which draws the y-axis labels
#' directly on the plot, and optionally drops the labels at the side.
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
#'
#' @return An annotation to be added to [ggplot2::ggplot()].
#' @export
#'
#' @examples
#' \dontrun{
#' gg_polar(mydata, "nox") + annotate_polar_axis(seq(5, 25, 5))
#' }
#'
annotate_polar_axis <- function(breaks, direction = "NW", drop = TRUE, ...) {
  x <- str_to_angle(direction)
  cross <- tidyr::crossing(x = x, y = breaks)
  out <-
    ggplot2::annotate(
      geom = "text",
      x = cross$x,
      y = cross$y,
      label = cross$y,
      ...
    )

  if (drop) {
    out <-
      list(
        out,
        ggplot2::theme(
          axis.text.y = ggplot2::element_blank(),
          axis.ticks.y = ggplot2::element_blank()
        )
      )
  }
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
