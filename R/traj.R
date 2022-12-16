#' Trajectory line plots with conditioning
#'
#' This function plots back trajectories. This function requires that data are
#' imported using the [openair::importTraj()] function.
#'
#' @param data Data frame, the result of importing a trajectory file using
#'   [openair::importTraj()].
#' @param lon Column containing the longitude, as a decimal.
#' @param lat Column containing the latitude, as a decimal.
#' @param colour A column to colour the paths by (for example, `"date"` or a
#'   pollutant like `"pm2.5"`).
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
#' @param lon_range,lat_range Longitude and latitude range, provided as a
#'   numeric vector of length 2.
#' @param crs The coordinate reference system (CRS) used for the base map.
#'   Defaults to `4326`, which behaves similarly to Cartesian coordinates. A useful
#'   alternative could be `3035` (the "lambert" projection).
#' @inheritDotParams ggplot2::coord_sf -crs -xlim -ylim
#' @family trajectory analysis functions
#' @seealso the `{openairmaps}` package for interactive trajectory maps using
#'   `{leaflet}`
#' @export

traj_plot <- function(data,
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
                      lon_range = NULL,
                      lat_range = NULL,
                      crs = 4326,
                      ...) {

  if (is.null(lon_range)) {
    lon_min <- min(data[[lon]], na.rm = TRUE)
    lon_max <- max(data[[lon]], na.rm = TRUE)
    lon_diff <- abs(lon_max - lon_min) * 0.3
    lon_range <- c(lon_min - lon_diff, lon_max + lon_diff)
  }
  if (is.null(lat_range)) {
    lat_min <- min(data[[lat]], na.rm = TRUE)
    lat_max <- max(data[[lat]], na.rm = TRUE)
    lat_diff <- abs(lat_max - lat_min) * 0.2
    lat_range <- c(lat_min - lat_diff, lat_max + lat_diff)
  }

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
    ggplot2::ggplot(data = data)

  if (map) {
    plt <- plt +
      ggplot2::geom_sf(
        data = world,
        fill = map_fill,
        colour = map_colour,
        alpha = map_alpha
      )
  }

  plt <- plt +
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
    ggplot2::coord_sf(xlim = lon_range,
                      ylim = lat_range,
                      crs = crs,
                      default_crs = 4326,
                      ...) +
    ggplot2::labs(x = NULL, y = NULL)
}

#' Trajectory level plots with conditioning
#'
#' This function plots grided back trajectories. This function requires that
#' data are imported using the [openair::importTraj()] function.
#' @inheritParams traj_plot
#' @param statistic By default the function will plot the trajectory
#'   frequencies. There are also various ways of plotting concentrations.
#'
#'   It is possible to set \code{statistic = "difference"}. In this case
#'   trajectories where the associated concentration is greater than
#'   \code{percentile} are compared with the the full set of trajectories to
#'   understand the differences in frequencies of the origin of air masses. The
#'   comparison is made by comparing the percentage change in gridded
#'   frequencies. For example, such a plot could show that the top 10\% of
#'   concentrations of PM10 tend to originate from air-mass origins to the east.
#'
#'   If \code{statistic = "pscf"} then a Potential Source Contribution Function
#'   map is produced. If \code{statistic = "cwt"} then concentration weighted
#'   trajectories are plotted.
#'
#'   If \code{statistic = "cwt"} then the Concentration Weighted Trajectory
#'   approach is used. See details.
#' @param pollutant By default, the trajectory height is summarised. `pollutant`
#'   overrides this, which is useful when `statistic` is something other than
#'   `"frequency"`.
#' @param lat_inc,lon_inc The latitude- and longitude-interval to be used for
#'   binning data.
#' @param min_bin The minimum number of unique points in a grid cell. Counts
#'   below `min_bin` are set as missing.
#' @param percentile For \code{trajLevel}. The percentile concentration of
#'   \code{pollutant} against which the all trajectories are compared.
#' @param sqtba_combine When statistic is "SQTBA" it is possible to combine lots
#'   of receptor locations to derive a single map. \code{.combine} identifies
#'   the column that differentiates different sites (commonly a column named
#'   \code{site}). Note that individual site maps are normalised first by
#'   dividing by their mean value.
#' @param sqtba_sigma For the SQTBA approach \code{sigma} determines the amount
#'   of back trajectory spread based on the Gaussian plume equation. Values in
#'   the literature suggest 5.4 km after one hour. However, testing suggests
#'   lower values reveal source regions more effectively while not introducing
#'   too much noise.
#' @param border_colour The colour to use for the border of each tile. Defaults
#'   to `NA`, which removes the border.
#' @param crs The coordinate reference system (CRS) used for the base map.
#'   Defaults to `4326`. Note that any other projections will likely run much
#'   slower.
#' @inheritDotParams ggplot2::coord_sf -crs -xlim -ylim
#' @family trajectory analysis functions
#' @family cluster analysis functions
#' @seealso the `{openairmaps}` package for interactive trajectory maps using
#'   `{leaflet}`
#' @export
traj_level <-
  function(data,
           lon = "lon",
           lat = "lat",
           statistic = "frequency",
           pollutant = "height",
           facet = NULL,
           lon_inc = 1,
           lat_inc = 1,
           min_bin = 1,
           percentile = 90,
           sqtba_combine = NA,
           sqtba_sigma = 1.5,
           border_colour = NA,
           map = TRUE,
           map_fill = "grey95",
           map_colour = "grey85",
           map_alpha = .5,
           lon_range = NULL,
           lat_range = NULL,
           crs = 4326,
           ...) {
    if (is.null(facet)) facet <- "default"

    if (is.null(lon_range)) {
      lon_min <- min(data[[lon]], na.rm = TRUE)
      lon_max <- max(data[[lon]], na.rm = TRUE)
      lon_diff <- abs(lon_max - lon_min) * 0.3
      lon_range <- c(lon_min - lon_diff, lon_max + lon_diff)
    }
    if (is.null(lat_range)) {
      lat_min <- min(data[[lat]], na.rm = TRUE)
      lat_max <- max(data[[lat]], na.rm = TRUE)
      lat_diff <- abs(lat_max - lat_min) * 0.2
      lat_range <- c(lat_min - lat_diff, lat_max + lat_diff)
    }

    if (statistic == "frequency") {
      title <- "percentage\ntrajectories"
      pollutant <- "default_pollutant"
      data[[pollutant]] <- pollutant
    }
    if (statistic == "difference") {
      lastnum <- stringr::str_sub(percentile, 2, 2)
      suff <- "th"
      if (lastnum == "1") {
        suff <- "st"
      }
      if (lastnum == "2") {
        suff <- "nd"
      }
      if (lastnum == "3") {
        suff <- "rd"
      }
      title <-
        stringr::str_glue("gridded\ndifferences\n({percentile}{suff} percentile)")
    }

    if (statistic == "pscf") {
      title <- "PSCF\nprobability"
    }
    if (statistic == "cwt") {
      title <- ""
    }
    if (statistic == "sqtba") {
      title <- stringr::str_glue("SQTBA\n{quick_text(pollutant)}")
    }

    data <- openair::trajLevel(
      mydata = data,
      lon = lon,
      lat = lat,
      type = facet,
      pollutant = pollutant,
      statistic = statistic,
      percentile = percentile,
      lat.inc = lat_inc,
      lon.inc = lon_inc,
      min.bin = min_bin,
      smooth = FALSE,
      .combine = sqtba_combine,
      sigma = sqtba_sigma,
      plot = FALSE
    )$data

    names(data)[names(data) == "height"] <- pollutant

    # make map

    plt <-
      ggplot2::ggplot(data = data)

    if (map) {
      plt <- plt +
        ggplot2::geom_sf(
          data = world,
          fill = map_fill,
          colour = map_colour,
          alpha = map_alpha
        )
    }

    plt <- plt +
      ggplot2::geom_rect(
        ggplot2::aes(
          xmin = .data$xgrid - (lon_inc / 2),
          xmax = .data$xgrid + (lon_inc / 2),
          ymin = .data$ygrid - (lat_inc / 2),
          ymax = .data$ygrid + (lat_inc / 2),
          fill = .data[[pollutant]]
        ),
        alpha = .5,
        color = border_colour
      ) +
      ggplot2::coord_sf(xlim = lon_range,
                        ylim = lat_range,
                        crs = crs,
                        default_crs = 4326,
                        ...) +
      ggplot2::labs(x = NULL, y = NULL, fill = title)

    if (any(facet != "default")) {
      if (length(facet) == 2) {
        plt <- plt + ggplot2::facet_grid(
          rows = ggplot2::vars(.data[[facet[1]]]),
          cols = ggplot2::vars(.data[[facet[2]]])
        )
      }
      if (length(facet) == 1) {
        plt <-
          plt + ggplot2::facet_wrap(facets = ggplot2::vars(.data[[facet]]))
      }
    }



    plt
  }

#' Calculate clusters for back trajectories
#'
#' This function carries out cluster analysis of HYSPLIT back trajectories. The
#' function is specifically designed to work with the trajectories imported
#' using the [openair::importTraj()] function, which provides pre-calculated
#' back trajectories at specific receptor locations. Clustered trajectories can
#' be visualised using [traj_plot()].
#'
#' @param data Data frame, the result of importing a trajectory file using
#'   [openair::importTraj()].
#' @param method Method used to calculate the distance matrix for the back
#'   trajectories. There are two methods available: "Euclid" and "Angle".
#' @param n_clusters Number of clusters to calculate.
#' @param facet One or two faceting columns. \code{facet} determines how the
#'   data are split and then clustered. Note that the cluster calculations are
#'   separately made of each level of "type".
#' @param split_after When \code{facet} != \code{NULL}, the trajectories can
#'   either be calculated for each facet independently or extracted after the
#'   cluster calculations have been applied to the whole data set.
#' @param return `"plot"` (the default) or `"data"`. `"plot"` will return
#'   plotted clusters for visual analysis so that an appropriate value for
#'   `n_clusters` can be selected. When such a value has been chosen, `"data"`
#'   will return the original data frame appended with a `cluster` column for
#'   use in, for example, [trend_prop()].
#' @inheritDotParams traj_plot -data -lat -lon -colour -facet
#' @family trajectory analysis functions
#' @export
traj_cluster <-
  function(data,
           method = "Euclid",
           n_clusters = 5,
           facet = NULL,
           split_after = FALSE,
           return = "plot",
           ...) {
    if (is.null(facet)) {
      facet <- "default"
    }
    out <-
      openair::trajCluster(
        traj = data,
        method = method,
        n.cluster = n_clusters,
        type = facet,
        split.after = split_after,
        plot = FALSE
      )$data

    out <- lapply(out, dplyr::ungroup)

    if (return == "plot") {
      top <- out$results %>%
        dplyr::group_by(.data$cluster, .data[[facet]]) %>%
        dplyr::slice_head(n = 1)
      if (facet == "default") facet <- NULL
      plt <-
        traj_plot(out$results, colour = "cluster", facet = facet, ...) +
        ggplot2::geom_text(data = top, ggplot2::aes(
          x = .data$lon,
          y = .data$lat,
          label = paste0(.data$freq, "%")
        ))
      return(plt)
    } else if (return == "data") {
      return(out$traj)
    }
  }
