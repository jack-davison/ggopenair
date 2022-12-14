% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/traj.R
\name{traj_level}
\alias{traj_level}
\title{Trajectory level plots with conditioning}
\usage{
traj_level(
  data,
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
  map_alpha = 0.5,
  lon_range = NULL,
  lat_range = NULL,
  crs = 4326,
  ...
)
}
\arguments{
\item{data}{Data frame, the result of importing a trajectory file using
\code{\link[openair:importTraj]{openair::importTraj()}}.}

\item{lon}{Column containing the longitude, as a decimal.}

\item{lat}{Column containing the latitude, as a decimal.}

\item{statistic}{By default the function will plot the trajectory
frequencies. There are also various ways of plotting concentrations.

It is possible to set \code{statistic = "difference"}. In this case
trajectories where the associated concentration is greater than
\code{percentile} are compared with the the full set of trajectories to
understand the differences in frequencies of the origin of air masses. The
comparison is made by comparing the percentage change in gridded
frequencies. For example, such a plot could show that the top 10\\% of
concentrations of PM10 tend to originate from air-mass origins to the east.

If \code{statistic = "pscf"} then a Potential Source Contribution Function
map is produced. If \code{statistic = "cwt"} then concentration weighted
trajectories are plotted.

If \code{statistic = "cwt"} then the Concentration Weighted Trajectory
approach is used. See details.}

\item{pollutant}{By default, the trajectory height is summarised. \code{pollutant}
overrides this, which is useful when \code{statistic} is something other than
\code{"frequency"}.}

\item{facet}{How to facet the plot.}

\item{lat_inc, lon_inc}{The latitude- and longitude-interval to be used for
binning data.}

\item{min_bin}{The minimum number of unique points in a grid cell. Counts
below \code{min_bin} are set as missing.}

\item{percentile}{For \code{trajLevel}. The percentile concentration of
\code{pollutant} against which the all trajectories are compared.}

\item{sqtba_combine}{When statistic is "SQTBA" it is possible to combine lots
of receptor locations to derive a single map. \code{.combine} identifies
the column that differentiates different sites (commonly a column named
\code{site}). Note that individual site maps are normalised first by
dividing by their mean value.}

\item{sqtba_sigma}{For the SQTBA approach \code{sigma} determines the amount
of back trajectory spread based on the Gaussian plume equation. Values in
the literature suggest 5.4 km after one hour. However, testing suggests
lower values reveal source regions more effectively while not introducing
too much noise.}

\item{border_colour}{The colour to use for the border of each tile. Defaults
to \code{NA}, which removes the border.}

\item{map}{Should a base map be drawn? If \code{TRUE} the world base map from
the \code{maps} package is used.}

\item{map_fill}{Fill colour of map.}

\item{map_colour}{Line colour of map.}

\item{map_alpha}{Alpha of map.}

\item{lon_range, lat_range}{Longitude and latitude range, provided as a
numeric vector of length 2.}

\item{crs}{The coordinate reference system (CRS) used for the base map.
Defaults to \code{4326}. Note that any other projections will likely run much
slower.}

\item{...}{
  Arguments passed on to \code{\link[ggplot2:ggsf]{ggplot2::coord_sf}}
  \describe{
    \item{\code{expand}}{If \code{TRUE}, the default, adds a small expansion factor to
the limits to ensure that data and axes don't overlap. If \code{FALSE},
limits are taken exactly from the data or \code{xlim}/\code{ylim}.}
    \item{\code{default_crs}}{The default CRS to be used for non-sf layers (which
don't carry any CRS information) and scale limits. The default value of
\code{NULL} means that the setting for \code{crs} is used. This implies that all
non-sf layers and scale limits are assumed to be specified in projected
coordinates. A useful alternative setting is \code{default_crs = sf::st_crs(4326)},
which means x and y positions are interpreted as longitude and latitude,
respectively, in the World Geodetic System 1984 (WGS84).}
    \item{\code{datum}}{CRS that provides datum to use when generating graticules.}
    \item{\code{label_graticule}}{Character vector indicating which graticule lines should be labeled
where. Meridians run north-south, and the letters \code{"N"} and \code{"S"} indicate that
they should be labeled on their north or south end points, respectively.
Parallels run east-west, and the letters \code{"E"} and \code{"W"} indicate that they
should be labeled on their east or west end points, respectively. Thus,
\code{label_graticule = "SW"} would label meridians at their south end and parallels at
their west end, whereas \code{label_graticule = "EW"} would label parallels at both
ends and meridians not at all. Because meridians and parallels can in general
intersect with any side of the plot panel, for any choice of \code{label_graticule} labels
are not guaranteed to reside on only one particular side of the plot panel. Also,
\code{label_graticule} can cause labeling artifacts, in particular if a graticule line
coincides with the edge of the plot panel. In such circumstances, \code{label_axes} will
generally yield better results and should be used instead.

This parameter can be used alone or in combination with \code{label_axes}.}
    \item{\code{label_axes}}{Character vector or named list of character values
specifying which graticule lines (meridians or parallels) should be labeled on
which side of the plot. Meridians are indicated by \code{"E"} (for East) and
parallels by \code{"N"} (for North). Default is \code{"--EN"}, which specifies
(clockwise from the top) no labels on the top, none on the right, meridians
on the bottom, and parallels on the left. Alternatively, this setting could have been
specified with \code{list(bottom = "E", left = "N")}.

This parameter can be used alone or in combination with \code{label_graticule}.}
    \item{\code{lims_method}}{Method specifying how scale limits are converted into
limits on the plot region. Has no effect when \code{default_crs = NULL}.
For a very non-linear CRS (e.g., a perspective centered
around the North pole), the available methods yield widely differing results, and
you may want to try various options. Methods currently implemented include \code{"cross"}
(the default), \code{"box"}, \code{"orthogonal"}, and \code{"geometry_bbox"}. For method \code{"cross"},
limits along one direction (e.g., longitude) are applied at the midpoint of the
other direction (e.g., latitude). This method avoids excessively large limits for
rotated coordinate systems but means that sometimes limits need to be expanded a
little further if extreme data points are to be included in the final plot region.
By contrast, for method \code{"box"}, a box is generated out of the limits along both directions,
and then limits in projected coordinates are chosen such that the entire box is
visible. This method can yield plot regions that are too large. Finally, method
\code{"orthogonal"} applies limits separately along each axis, and method
\code{"geometry_bbox"} ignores all limit information except the bounding boxes of any
objects in the \code{geometry} aesthetic.}
    \item{\code{ndiscr}}{Number of segments to use for discretising graticule lines;
try increasing this number when graticules look incorrect.}
    \item{\code{default}}{Is this the default coordinate system? If \code{FALSE} (the default),
then replacing this coordinate system with another one creates a message alerting
the user that the coordinate system is being replaced. If \code{TRUE}, that warning
is suppressed.}
    \item{\code{clip}}{Should drawing be clipped to the extent of the plot panel? A
setting of \code{"on"} (the default) means yes, and a setting of \code{"off"}
means no. In most cases, the default of \code{"on"} should not be changed,
as setting \code{clip = "off"} can cause unexpected results. It allows
drawing of data points anywhere on the plot, including in the plot margins. If
limits are set via \code{xlim} and \code{ylim} and some data points fall outside those
limits, then those data points may show up in places such as the axes, the
legend, the plot title, or the plot margins.}
  }}
}
\description{
This function plots grided back trajectories. This function requires that
data are imported using the \code{\link[openair:importTraj]{openair::importTraj()}} function.
}
\seealso{
the \code{{openairmaps}} package for interactive trajectory maps using
\code{{leaflet}}

Other trajectory analysis functions: 
\code{\link{traj_cluster}()},
\code{\link{traj_plot}()}

Other cluster analysis functions: 
\code{\link{polar_cluster}()},
\code{\link{trend_prop}()}
}
\concept{cluster analysis functions}
\concept{trajectory analysis functions}
