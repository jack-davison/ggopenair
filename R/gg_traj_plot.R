#' Trajectory line plots with conditioning
#'
#' This function plots back trajectories. This function requires that data are
#' imported using the [openair::importTraj()] function.
#'
#' @param data Data frame, the result of importing a trajectory file using
#'   [openair::importTraj()].
#' @param lon Column containing the longitude, as a decimal.
#' @param lat Column containing the latitude, as a decimal.
#' @param colour A column to colour the paths by.
#' @param facet How to facet the plot.
#' @param alpha Transparency of the plots/lines, where `1` is completely opaque
#'   and `0` is completely transparent.
#' @param n_points A dot is placed every \code{n_points} along each full
#'   trajectory. For hourly back trajectories points are plotted every
#'   \code{npoint} hours. This helps to understand where the air masses were at
#'   particular times and get a feel for the speed of the air (points closer
#'   togther correspond to slower moving air masses). If \code{n_points = NULL}
#'   then no points are added.
#' @param map Should a base map be drawn? If \code{TRUE} the world base map from
#'   the \code{maps} package is used.
#' @param map_fill Fill colour of map.
#' @param map_colour Line colour of map.
#' @param map_alpha Alpha of map.
#' @param projection The map projection to be used. Different map projections
#'   are possible through the \code{mapproj} package. See \code{?mapproject} for
#'   extensive details and information on setting other parameters and
#'   orientation (see below).
#' @param ... other arguments that are passed to [ggplot2::coord_map()].
#' @export

gg_traj_plot <- function(data,
                         lon = "lon",
                         lat = "lat",
                         colour = NULL,
                         facet = NULL,
                         alpha = 1,
                         n_points = 12,
                         map = TRUE,
                         map_fill = "grey95",
                         map_colour = "grey85",
                         map_alpha = .5,
                         projection = "mercator",
                         ...) {
  londiff <-
    (max(data[[lon]], na.rm = T) - min(data[[lon]], na.rm = T))
  lonrange <-
    c(
      min(data[[lon]], na.rm = T) - londiff * 0.1,
      max(data[[lon]], na.rm = T) + londiff * 0.1
    )
  biglonrange <-
    c(min(data[[lon]], na.rm = T) - londiff * 2, max(data[[lon]], na.rm = T) + londiff * 2)

  latdiff <-
    (max(data[[lat]], na.rm = T) - min(data[[lat]], na.rm = T)) * 0.1
  latrange <-
    c(
      min(data[[lat]], na.rm = T) - latdiff * 0.1,
      max(data[[lat]], na.rm = T) + latdiff * 0.1
    )
  biglatrange <-
    c(
      min(data[[lat]], na.rm = T) - latdiff * 2,
      max(data[[lat]], na.rm = T) + latdiff * 2
    )

  world <- maps::map("world", plot = FALSE, fill = TRUE) %>%
    maptools::pruneMap(xlim = biglonrange, ylim = biglatrange) %>%
    broom::tidy()

  if (is.null(colour)) {
    aesthetics <-
      ggplot2::aes(.data[[lon]], .data[[lat]], group = .data$date)
  } else {
    aesthetics <-
      ggplot2::aes(.data[[lon]], .data[[lat]], group = .data$date, colour = .data[[colour]])
    data <- dplyr::arrange(data, .data[[colour]])
  }

  plt <-
    ggplot2::ggplot(data = data) +
    ggplot2::geom_polygon(
      data = world,
      fill = map_fill,
      colour = map_colour,
      alpha = map_alpha,
      ggplot2::aes(.data$long, .data$lat, group = .data$group)
    ) +
    ggplot2::geom_path(mapping = aesthetics, alpha = alpha)

  if (!is.null(n_points)) {
    plt <- plt + ggplot2::geom_point(
      data = dplyr::filter(data, .data$hour.inc %% n_points == 0),
      aesthetics,
      alpha = alpha
    )
  }

  if (!is.null(facet)) {
    if (length(facet) == 2) {
      plt <- plt + ggplot2::facet_grid(
        rows = ggplot2::vars(.data[[facet[1]]]),
        cols = ggplot2::vars(.data[[facet[2]]])
      )
    }
    if (length(facet) == 1) {
      plt <- plt + ggplot2::facet_wrap(facets = ggplot2::vars(.data[[facet]]))
    }
  }

  plt +
    ggplot2::coord_map(
      projection = projection,
      xlim = lonrange,
      ylim = latrange,
      ...
    ) +
    ggplot2::labs(x = NULL, y = NULL)
}
