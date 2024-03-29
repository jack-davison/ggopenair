% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/polar_plot.R
\name{polar_cluster}
\alias{polar_cluster}
\title{K-means clustering of bivariate polar plots}
\usage{
polar_cluster(
  data,
  pollutant,
  x = "ws",
  wd = "wd",
  n_clusters = 6,
  data_after = NA,
  return = "plot",
  ...
)
}
\arguments{
\item{data}{A data frame containing wind direction, wind speed, and pollutant
concentrations.}

\item{pollutant}{One or more column names identifying pollutant
concentrations. When multiple pollutants are specified for a
single-pollutant \code{statistic} (e.g., "mean"), a faceted plot will be
returned. Two pollutants must be provided for certain \code{statistic}
options (e.g., "Pearson" in \code{\link[=polar_plot]{polar_plot()}}).}

\item{x}{Name of variable to plot against wind direction in polar
coordinates, the default is wind speed, \dQuote{ws}.}

\item{wd}{Name of wind direction field.}

\item{n_clusters}{Number of clusters to use. If \code{n_clusters} is more than
length 1, then a faceted plot will be output showing the clusters
identified for each one of \code{n_clusters}.}

\item{data_after}{Optional. Data representing the "after" case; see
\code{\link[=polar_diff]{polar_diff()}} for more information.}

\item{return}{\code{"plot"} (the default) or \code{"data"}. \code{"plot"} will return
plotted clusters for visual analysis so that an appropriate value for
\code{n_clusters} can be selected. When such a value has been chosen, \code{"data"}
will return the original data frame appended with a \code{cluster} column for
use in, for example, \code{\link[=trend_prop]{trend_prop()}}.}

\item{...}{
  Arguments passed on to \code{\link[openair:polarPlot]{openair::polarPlot}}
  \describe{
    \item{\code{statistic}}{The statistic that should be applied to each wind
speed/direction bin. Because of the smoothing involved, the colour scale
for some of these statistics is only to provide an indication of overall
pattern and should not be interpreted in concentration units e.g. for
\code{statistic = "weighted.mean"} where the bin mean is multiplied by the
bin frequency and divided by the total frequency. In many cases using
\code{polarFreq} will be better. Setting \code{statistic = "weighted.mean"}
can be useful because it provides an indication of the concentration *
frequency of occurrence and will highlight the wind speed/direction
conditions that dominate the overall mean.Can be:

\itemize{ \item  \dQuote{mean} (default), \dQuote{median}, \dQuote{max}
(maximum), \dQuote{frequency}. \dQuote{stdev} (standard deviation),
\dQuote{weighted.mean}.

\item \code{statistic = "nwr"} Implements the Non-parametric Wind
Regression approach of Henry et al. (2009) that uses kernel smoothers. The
\code{openair} implementation is not identical because Gaussian kernels are
used for both wind direction and speed. The smoothing is controlled by
\code{ws_spread} and \code{wd_spread}.

\item \code{statistic = "cpf"} the conditional probability function (CPF)
is plotted and a single (usually high) percentile level is supplied. The
CPF is defined as CPF = my/ny, where my is the number of samples in the y
bin (by default a wind direction, wind speed interval) with mixing ratios
greater than the \emph{overall} percentile concentration, and ny is the
total number of samples in the same wind sector (see Ashbaugh et al.,
1985). Note that percentile intervals can also be considered; see
\code{percentile} for details.

\item When \code{statistic = "r"} or \code{statistic = "Pearson"}, the
Pearson correlation coefficient is calculated for \emph{two} pollutants.
The calculation involves a weighted Pearson correlation coefficient, which
is weighted by Gaussian kernels for wind direction an the radial variable
(by default wind speed). More weight is assigned to values close to a wind
speed-direction interval. Kernel weighting is used to ensure that all data
are used rather than relying on the potentially small number of values in a
wind speed-direction interval.

\item When \code{statistic = "Spearman"}, the Spearman correlation
coefficient is calculated for \emph{two} pollutants. The calculation
involves a weighted Spearman correlation coefficient, which is weighted by
Gaussian kernels for wind direction an the radial variable (by default wind
speed). More weight is assigned to values close to a wind speed-direction
interval. Kernel weighting is used to ensure that all data are used rather
than relying on the potentially small number of values in a wind
speed-direction interval.

\item \code{"robust_slope"} is another option for pair-wise statistics and
\code{"quantile.slope"}, which uses quantile regression to estimate the
slope for a particular quantile level (see also \code{tau} for setting the
quantile level).

\item \code{"york_slope"} is another option for pair-wise statistics which
uses the \emph{York regression method} to estimate the slope. In this
method the uncertainties in \code{x} and \code{y} are used in the
determination of the slope. The uncertainties are provided by
\code{x_error} and \code{y_error} --- see below.}}
    \item{\code{exclude.missing}}{Setting this option to \code{TRUE} (the default)
removes points from the plot that are too far from the original data. The
smoothing routines will produce predictions at points where no data exist
i.e. they predict. By removing the points too far from the original data
produces a plot where it is clear where the original data lie. If set to
\code{FALSE} missing data will be interpolated.}
    \item{\code{uncertainty}}{Should the uncertainty in the calculated surface be shown?
If \code{TRUE} three plots are produced on the same scale showing the
predicted surface together with the estimated lower and upper uncertainties
at the 95\% confidence interval. Calculating the uncertainties is useful to
understand whether features are real or not.  For example, at high wind
speeds where there are few data there is greater uncertainty over the
predicted values. The uncertainties are calculated using the GAM and
weighting is done by the frequency of measurements in each wind
speed-direction bin. Note that if uncertainties are calculated then the
type is set to "default".}
    \item{\code{percentile}}{If \code{statistic = "percentile"} then \code{percentile}
is used, expressed from 0 to 100. Note that the percentile value is
calculated in the wind speed, wind direction \sQuote{bins}. For this reason
it can also be useful to set \code{min.bin} to ensure there are a
sufficient number of points available to estimate a percentile. See
\code{quantile} for more details of how percentiles are calculated.

\code{percentile} is also used for the Conditional Probability Function
(CPF) plots. \code{percentile} can be of length two, in which case the
percentile \emph{interval} is considered for use with CPF. For example,
\code{percentile = c(90, 100)} will plot the CPF for concentrations between
the 90 and 100th percentiles. Percentile intervals can be useful for
identifying specific sources. In addition, \code{percentile} can also be of
length 3. The third value is the \sQuote{trim} value to be applied. When
calculating percentile intervals many can cover very low values where there
is no useful information. The trim value ensures that values greater than
or equal to the trim * mean value are considered \emph{before} the
percentile intervals are calculated. The effect is to extract more detail
from many source signatures. See the manual for examples. Finally, if the
trim value is less than zero the percentile range is interpreted as
absolute concentration values and subsetting is carried out directly.}
    \item{\code{cols}}{Colours to be used for plotting. Options include
\dQuote{default}, \dQuote{increment}, \dQuote{heat}, \dQuote{jet} and
\code{RColorBrewer} colours --- see the \code{openair} \code{openColours}
function for more details. For user defined the user can supply a list of
colour names recognised by R (type \code{colours()} to see the full list).
An example would be \code{cols = c("yellow", "green", "blue")}. \code{cols}
can also take the values \code{"viridis"}, \code{"magma"},
\code{"inferno"}, or \code{"plasma"} which are the viridis colour maps
ported from Python's Matplotlib library.}
    \item{\code{weights}}{At the edges of the plot there may only be a few data points
in each wind speed-direction interval, which could in some situations
distort the plot if the concentrations are high. \code{weights} applies a
weighting to reduce their influence. For example and by default if only a
single data point exists then the weighting factor is 0.25 and for two
points 0.5. To not apply any weighting and use the data as is, use
\code{weights = c(1, 1, 1)}.

An alternative to down-weighting these points they can be removed
altogether using \code{min.bin}.}
    \item{\code{min.bin}}{The minimum number of points allowed in a wind speed/wind
direction bin.  The default is 1. A value of two requires at least 2 valid
records in each bin an so on; bins with less than 2 valid records are set
to NA. Care should be taken when using a value > 1 because of the risk of
removing real data points. It is recommended to consider your data with
care. Also, the \code{polarFreq} function can be of use in such
circumstances.}
    \item{\code{force.positive}}{The default is \code{TRUE}. Sometimes if smoothing data
with steep gradients it is possible for predicted values to be negative.
\code{force.positive = TRUE} ensures that predictions remain positive. This
is useful for several reasons. First, with lots of missing data more
interpolation is needed and this can result in artefacts because the
predictions are too far from the original data. Second, if it is known
beforehand that the data are all positive, then this option carries that
assumption through to the prediction. The only likely time where setting
\code{force.positive = FALSE} would be if background concentrations were
first subtracted resulting in data that is legitimately negative. For the
vast majority of situations it is expected that the user will not need to
alter the default option.}
    \item{\code{k}}{This is the smoothing parameter used by the \code{gam} function in
package \code{mgcv}. Typically, value of around 100 (the default) seems to
be suitable and will resolve important features in the plot. The most
appropriate choice of \code{k} is problem-dependent; but extensive testing
of polar plots for many different problems suggests a value of \code{k} of
about 100 is suitable. Setting \code{k} to higher values will not tend to
affect the surface predictions by much but will add to the computation
time. Lower values of \code{k} will increase smoothing. Sometimes with few
data to plot \code{polarPlot} will fail. Under these circumstances it can
be worth lowering the value of \code{k}.}
    \item{\code{normalise}}{If \code{TRUE} concentrations are normalised by dividing by
their mean value. This is done \emph{after} fitting the smooth surface.
This option is particularly useful if one is interested in the patterns of
concentrations for several pollutants on different scales e.g. NOx and CO.
Often useful if more than one \code{pollutant} is chosen.}
    \item{\code{key.footer}}{see \code{key.footer}.}
    \item{\code{key.position}}{Location where the scale key is to plotted. Allowed
arguments currently include \code{"top"}, \code{"right"}, \code{"bottom"}
and \code{"left"}.}
    \item{\code{ws_spread}}{The value of sigma used for Gaussian kernel weighting of
wind speed when \code{statistic = "nwr"} or when correlation and regression
statistics are used such as \emph{r}. Default is \code{0.5}.}
    \item{\code{wd_spread}}{The value of sigma used for Gaussian kernel weighting of
wind direction when \code{statistic = "nwr"} or when correlation and
regression statistics are used such as \emph{r}. Default is \code{4}.}
    \item{\code{x_error}}{The \code{x} error / uncertainty used when \code{statistic =
"york_slope"}.}
    \item{\code{y_error}}{The \code{y} error / uncertainty used when \code{statistic =
"york_slope"}.}
    \item{\code{kernel}}{Type of kernel used for the weighting procedure for when
correlation or regression techniques are used. Only \code{"gaussian"} is
supported but this may be enhanced in the future.}
    \item{\code{tau}}{The quantile to be estimated when \code{statistic} is set to
\code{"quantile.slope"}. Default is \code{0.5} which is equal to the median
and will be ignored if \code{"quantile.slope"} is not used.}
  }}
}
\description{
Function for identifying clusters in bivariate polar plots (\code{\link[=polarPlot]{polarPlot()}});
identifying clusters in the original data for subsequent processing.
}
\details{
Bivariate polar plots generated using the \code{\link[=polarPlot]{polarPlot()}} function provide a
very useful graphical technique for identifying and characterising different
air pollution sources. While bivariate polar plots provide a useful graphical
indication of potential sources, their location and wind-speed or other
variable dependence, they do have several limitations. Often, a `feature'
will be detected in a plot but the subsequent analysis of data meeting
particular wind speed/direction criteria will be based only on the judgement
of the investigator concerning the wind speed-direction intervals of
interest. Furthermore, the identification of a feature can depend on the
choice of the colour scale used, making the process somewhat arbitrary.

\code{polarCluster} applies Partition Around Medoids (PAM) clustering
techniques to \code{\link[=polarPlot]{polarPlot()}} surfaces to help identify potentially interesting
features for further analysis. Details of PAM can be found in the
\code{cluster} package (a core R package that will be pre-installed on all R
systems). PAM clustering is similar to k-means but has several advantages
e.g. is more robust to outliers. The clustering is based on the equal
contribution assumed from the u and v wind components and the associated
concentration. The data are standardized before clustering takes place.

The function works best by first trying different numbers of clusters and
plotting them. This is achieved by setting \code{n_clusters} to be of length
more than 1. For example, if \code{n_clusters = 2:10} then a plot will be
output showing the 9 cluster levels 2 to 10.

The clustering can also be applied to differences in polar plot surfaces (see
\code{\link[=polarDiff]{polarDiff()}}). On this case a second data frame (\code{after}) should be
supplied.

Note that clustering is computationally intensive and the function can take a
long time to run --- particularly when the number of clusters is increased.
For this reason it can be a good idea to run a few clusters first to get a
feel for it, e.g., \code{n_clusters = 2:5}.

Once the number of clusters has been decided, the user can then run
\code{\link[=polar_cluster]{polar_cluster()}} to return the original data frame together with a new
column \code{cluster}, which gives the cluster number as a character (see
example). Note that any rows where the value of \code{pollutant} is \code{NA} are
ignored so that the returned data frame may have fewer rows than the
original.

Note that there are no automatic ways in ensuring the most appropriate number
of clusters as this is application dependent. However, there is often
a-priori information available on what different features in polar plots
correspond to. Nevertheless, the appropriateness of different clusters is
best determined by post-processing the data. The Carslaw and Beevers (2012)
paper discusses these issues in more detail.
}
\references{
Carslaw, D.C., Beevers, S.D, Ropkins, K and M.C. Bell (2006). Detecting and
quantifying aircraft and other on-airport contributions to ambient nitrogen
oxides in the vicinity of a large international airport.  Atmospheric
Environment. 40/28 pp 5424-5434.

Carslaw, D.C., & Beevers, S.D. (2013). Characterising and understanding
emission sources using bivariate polar plots and k-means clustering.
Environmental Modelling & Software, 40, 325-329.
doi:10.1016/j.envsoft.2012.09.005
}
\seealso{
Other polar directional analysis functions: 
\code{\link{polar_annulus}()},
\code{\link{polar_diff}()},
\code{\link{polar_freq}()},
\code{\link{polar_plot}()},
\code{\link{rose_metbias}()},
\code{\link{rose_percentile}()},
\code{\link{rose_pollution}()},
\code{\link{rose_wind}()}

Other cluster analysis functions: 
\code{\link{trend_prop}()}
}
\concept{cluster analysis functions}
\concept{polar directional analysis functions}
