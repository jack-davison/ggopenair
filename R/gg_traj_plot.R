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
#'   together correspond to slower moving air masses). If \code{n_points = NULL}
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
#' @inheritDotParams ggplot2::coord_map -projection -xlim -ylim
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
    (max(data[[lon]], na.rm = TRUE) - min(data[[lon]], na.rm = TRUE))
  lonrange <-
    c(min(data[[lon]], na.rm = TRUE) - londiff * 0.1,
      max(data[[lon]], na.rm = TRUE) + londiff * 0.1)
  biglonrange <-
    c(min(data[[lon]], na.rm = TRUE) - londiff * 2,
      max(data[[lon]], na.rm = TRUE) + londiff * 2)

  latdiff <-
    (max(data[[lat]], na.rm = TRUE) - min(data[[lat]], na.rm = TRUE)) * 0.1
  latrange <-
    c(min(data[[lat]], na.rm = TRUE) - latdiff * 0.1,
      max(data[[lat]], na.rm = TRUE) + latdiff * 0.1)
  biglatrange <-
    c(min(data[[lat]], na.rm = TRUE) - latdiff * 2,
      max(data[[lat]], na.rm = TRUE) + latdiff * 2)

  world <- maps::map("world", plot = FALSE, fill = TRUE) %>%
    maptools::pruneMap(xlim = biglonrange, ylim = biglatrange) %>%
    broom::tidy()

  if (is.null(colour)) {
    aesthetics <-
      ggplot2::aes(.data[[lon]], .data[[lat]], group = .data$date)
  } else {
    aesthetics <-
      ggplot2::aes(.data[[lon]], .data[[lat]],
                   group = .data$date, colour = .data[[colour]])
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
      plt <- plt + ggplot2::facet_grid(rows = ggplot2::vars(.data[[facet[1]]]),
                                       cols = ggplot2::vars(.data[[facet[2]]]))
    }
    if (length(facet) == 1) {
      plt <-
        plt + ggplot2::facet_wrap(facets = ggplot2::vars(.data[[facet]]))
    }
  }

  plt +
    ggplot2::coord_map(projection = projection,
                       xlim = lonrange,
                       ylim = latrange,
                       ...) +
    ggplot2::labs(x = NULL, y = NULL)
}

#' Calculate clusters for back trajectories
#'
#' This function carries out cluster analysis of HYSPLIT back trajectories. The
#' function is specifically designed to work with the trajectories imported
#' using the [openair::importTraj()] function, which provides pre-calculated
#' back trajectories at specific receptor locations. Clustered trajectories can
#' be visualised using [gg_traj_plot()].
#'
#' @param data Data frame, the result of importing a trajectory file using
#'   [openair::importTraj()].
#' @param method Method used to calculate the distance matrix for the back
#'   trajectories. There are two methods available: "Euclid" and "Angle".
#' @param n_cluster Number of clusters to calculate.
#' @param facet One or two faceting columns. \code{facet} determines how the
#'   data are split and then clustered. Note that the cluster calculations are
#'   separately made of each level of "type".
#' @param split_after When \code{facet} != \code{NULL}, the trajectories can
#'   either be calculated for each facet independently or extracted after the
#'   cluster calculations have been applied to the whole data set.
#' @param plot Automatically plot the clustered trajectories? Defaults to
#'   \code{FALSE}. When \code{TRUE}, a [patchwork][patchwork::patchwork-package]
#'   assemblage is returned. The plots can be controlled using `...`.
#'   Alternatively, the output of [traj_cluster()] can be manually passed to
#'   [gg_traj_plot()].
#' @inheritDotParams gg_traj_plot -data -lat -lon -colour -facet
#' @export
traj_cluster <-
  function(data,
           method = "Euclid",
           n_cluster = 5,
           facet = NULL,
           split_after = FALSE,
           plot = FALSE,
           ...) {
    if (is.null(facet))
      facet <- "default"
    out <-
      openair::trajCluster(
        traj = data,
        method = "method",
        n.cluster = n_cluster,
        type = facet,
        split.after = split_after,
        plot = FALSE
      )$data

    out <- lapply(out, dplyr::ungroup)

    if (plot) {
      if (facet == "default") facet <- NULL
      plt <-
        patchwork::wrap_plots(
          gg_traj_plot(out$traj, colour = "cluster", facet = facet, ...),
          gg_traj_plot(out$results, colour = "cluster", facet = facet, ...)
        ) +
        patchwork::plot_layout(guides = "collect")
      return(plt)
    } else {
      return(out)
    }
  }
