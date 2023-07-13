#' Function for plotting bivariate polar plots with smoothing.
#'
#' Function for plotting pollutant concentration in polar coordinates showing
#' concentration by wind speed (or another numeric variable) and direction. Mean
#' concentrations are calculated for wind speed-direction \sQuote{bins} (e.g.
#' 0-1, 1-2 m/s,...  and 0-10, 10-20 degrees etc.).  To aid interpretation,
#' \code{gam} smoothing is carried out using \code{mgcv}.
#'
#' The bivariate polar plot is a useful diagnostic tool for quickly gaining an
#' idea of potential sources. Wind speed is one of the most useful variables to
#' use to separate source types (see references). For example, ground-level
#' concentrations resulting from buoyant plumes from chimney stacks tend to peak
#' under higher wind speed conditions. Conversely, ground-level, non-buoyant
#' plumes such as from road traffic, tend to have highest concentrations under
#' low wind speed conditions. Other sources such as from aircraft engines also
#' show differing characteristics by wind speed.
#'
#' The function has been developed to allow variables other than wind speed to
#' be plotted with wind direction in polar coordinates. The key issue is that
#' the other variable plotted against wind direction should be discriminating in
#' some way. For example, temperature can help reveal high-level sources brought
#' down to ground level in unstable atmospheric conditions, or show the effect a
#' source emission dependent on temperature, e.g., biogenic isoprene.
#'
#' The plots can vary considerably depending on how much smoothing is done.  The
#' approach adopted here is based on the very flexible and capable \code{mgcv}
#' package that uses \emph{Generalized Additive Models}. While methods do exist
#' to find an optimum level of smoothness, they are not necessarily useful. The
#' principal aim of \code{polarPlot} is as a graphical analysis rather than for
#' quantitative purposes. In this respect the smoothing aims to strike a balance
#' between revealing interesting (real) features and overly noisy data. The
#' defaults used in \code{polarPlot} are based on the analysis of data from many
#' different sources. More advanced users may wish to modify the code and adopt
#' other smoothing approaches.
#'
#' Various statistics are possible to consider e.g. mean, maximum, median.
#' \code{statistic = "max"} is often useful for revealing sources. Pair-wise
#' statistics between two pollutants can also be calculated.
#'
#' Because of the smoothing involved, the colour scale for some of these
#' statistics is only to provide an indication of overall pattern and should not
#' be interpreted in concentration units, e.g., for \code{statistic =
#' "weighted.mean"} where the bin mean is multiplied by the bin frequency and
#' divided by the total frequency. In many cases using \code{polarFreq} will be
#' better. Setting \code{statistic = "weighted.mean"} can be useful because it
#' provides an indication of the concentration * frequency of occurrence and
#' will highlight the wind speed/direction conditions that dominate the overall
#' mean.
#'
#' The function can also be used to compare two pollutant species through a
#' range of pair-wise statistics (see help on \code{statistic}) and Grange et
#' al. (2016) (open-access publication link below).
#'
#' Wind direction is split up into 10 degree intervals and the other variable
#' (e.g. wind speed) 30 intervals. These 2D bins are then used to calculate the
#' statistics.
#'
#' These plots often show interesting features at higher wind speeds (see
#' references below). For these conditions there can be very few measurements
#' and therefore greater uncertainty in the calculation of the surface. There
#' are several ways in which this issue can be tackled. First, it is possible to
#' avoid smoothing altogether and use \code{polarFreq} in the package
#' \code{openair}. Second, the effect of setting a minimum number of
#' measurements in each wind speed-direction bin can be examined through
#' \code{min_bin}. It is possible that a single point at high wind speed
#' conditions can strongly affect the surface prediction. Therefore, setting
#' \code{min_bin = 3}, for example, will remove all wind speed-direction bins
#' with fewer than 3 measurements \emph{before} fitting the surface. Third,
#' consider setting \code{uncertainty = TRUE}. This option will show the
#' predicted surface together with upper and lower 95% confidence intervals,
#' which take account of the frequency of measurements.
#'
#' @param data A data frame containing wind direction, wind speed, and pollutant
#'   concentrations.
#' @param pollutant One or more column names identifying pollutant
#'   concentrations. When multiple pollutants are specified for a
#'   single-pollutant \code{statistic} (e.g., "mean"), a faceted plot will be
#'   returned. Two pollutants must be provided for certain \code{statistic}
#'   options (e.g., "Pearson" in [polar_plot()]).
#' @param x Name of variable to plot against wind direction in polar
#'   coordinates, the default is wind speed, \dQuote{ws}.
#' @param wd Name of wind direction field.
#' @param facet One or two faceting columns. \code{facet} determines how the
#'   data are split and then plotted. When \code{facet} is length 1 it is passed
#'   to [ggplot2::facet_wrap()], and when it is length 2 it is passed to
#'   [ggplot2::facet_grid()] with the first element being used as columns and
#'   the second rows. Some other options (e.g., multiple \code{pollutant}
#'   columns) can limit the the number of faceting columns to 1.
#' @param statistic The statistic that should be applied to each wind
#'   speed/direction bin. Can be:
#'
#'   \itemize{ \item  \dQuote{mean} (default), \dQuote{median}, \dQuote{max}
#'   (maximum), \dQuote{frequency}. \dQuote{stdev} (standard deviation),
#'   \dQuote{weighted.mean}.
#'
#'   \item \code{statistic = "nwr"} Implements the Non-parametric Wind
#'   Regression approach of Henry et al. (2009) that uses kernel smoothers. The
#'   \code{openair} implementation is not identical because Gaussian kernels are
#'   used for both wind direction and speed. The smoothing is controlled by
#'   \code{ws_spread} and \code{wd_spread}.
#'
#'   \item \code{statistic = "cpf"} the conditional probability function (CPF)
#'   is plotted and a single (usually high) percentile level is supplied. The
#'   CPF is defined as CPF = my/ny, where my is the number of samples in the y
#'   bin (by default a wind direction, wind speed interval) with mixing ratios
#'   greater than the \emph{overall} percentile concentration, and ny is the
#'   total number of samples in the same wind sector (see Ashbaugh et al.,
#'   1985). Note that percentile intervals can also be considered; see
#'   \code{percentile} for details.
#'
#'   \item When \code{statistic = "r"} or \code{statistic = "Pearson"}, the
#'   Pearson correlation coefficient is calculated for \emph{two} pollutants.
#'   The calculation involves a weighted Pearson correlation coefficient, which
#'   is weighted by Gaussian kernels for wind direction an the radial variable
#'   (by default wind speed). More weight is assigned to values close to a wind
#'   speed-direction interval. Kernel weighting is used to ensure that all data
#'   are used rather than relying on the potentially small number of values in a
#'   wind speed-direction interval.
#'
#'   \item When \code{statistic = "Spearman"}, the Spearman correlation
#'   coefficient is calculated for \emph{two} pollutants. The calculation
#'   involves a weighted Spearman correlation coefficient, which is weighted by
#'   Gaussian kernels for wind direction an the radial variable (by default wind
#'   speed). More weight is assigned to values close to a wind speed-direction
#'   interval. Kernel weighting is used to ensure that all data are used rather
#'   than relying on the potentially small number of values in a wind
#'   speed-direction interval.
#'
#'   \item \code{"robust_slope"} is another option for pair-wise statistics and
#'   \code{"quantile.slope"}, which uses quantile regression to estimate the
#'   slope for a particular quantile level (see also \code{tau} for setting the
#'   quantile level).
#'
#'   \item \code{"york_slope"} is another option for pair-wise statistics which
#'   uses the \emph{York regression method} to estimate the slope. In this
#'   method the uncertainties in \code{x} and \code{y} are used in the
#'   determination of the slope. The uncertainties are provided by
#'   \code{x_error} and \code{y_error} --- see below.}
#' @param exclude_missing Setting this option to \code{TRUE} (the default)
#'   removes points from the plot that are too far from the original data. The
#'   smoothing routines will produce predictions at points where no data exist,
#'   i.e., they predict by removing the points too far from the original data
#'   produces a plot where it is clear where the original data lie. If set to
#'   \code{FALSE} missing data will be interpolated.
#' @param uncertainty Should the uncertainty in the calculated surface be shown?
#'   If \code{TRUE} three plots are produced on the same scale showing the
#'   predicted surface together with the estimated lower and upper uncertainties
#'   at the 95% confidence interval. Calculating the uncertainties is useful to
#'   understand whether features are real or not.  For example, at high wind
#'   speeds where there are few data there is greater uncertainty over the
#'   predicted values. The uncertainties are calculated using the GAM and
#'   weighting is done by the frequency of measurements in each wind
#'   speed-direction bin. Note that if uncertainties are calculated then the
#'   type is set to "default".
#' @param percentile If \code{statistic = "percentile"} then \code{percentile}
#'   is used, expressed from 0 to 100. Note that the percentile value is
#'   calculated in the wind speed, wind direction \sQuote{bins}. For this reason
#'   it can also be useful to set \code{min_bin} to ensure there are a
#'   sufficient number of points available to estimate a percentile. See
#'   \code{quantile} for more details of how percentiles are calculated.
#'
#'   \code{percentile} is also used for the Conditional Probability Function
#'   (CPF) plots. \code{percentile} can be of length two, in which case the
#'   percentile \emph{interval} is considered for use with CPF. For example,
#'   \code{percentile = c(90, 100)} will plot the CPF for concentrations between
#'   the 90 and 100th percentiles. Percentile intervals can be useful for
#'   identifying specific sources. In addition, \code{percentile} can also be of
#'   length 3. The third value is the \sQuote{trim} value to be applied. When
#'   calculating percentile intervals many can cover very low values where there
#'   is no useful information. The trim value ensures that values greater than
#'   or equal to the trim * mean value are considered \emph{before} the
#'   percentile intervals are calculated. The effect is to extract more detail
#'   from many source signatures. See the manual for examples. Finally, if the
#'   trim value is less than zero the percentile range is interpreted as
#'   absolute concentration values and subsetting is carried out directly.
#' @param weights At the edges of the plot there may only be a few data points
#'   in each wind speed-direction interval, which could in some situations
#'   distort the plot if the concentrations are high. \code{weights} applies a
#'   weighting to reduce their influence. For example and by default if only a
#'   single data point exists then the weighting factor is 0.25 and for two
#'   points 0.5. To not apply any weighting and use the data as is, use
#'   \code{weights = c(1, 1, 1)}. An alternative to down-weighting these points
#'   they can be removed altogether using \code{min_bin}.
#' @param min_bin The minimum number of points allowed in a wind speed/wind
#'   direction bin.  The default is 1. A value of two requires at least 2 valid
#'   records in each bin an so on; bins with less than 2 valid records are set
#'   to NA. Care should be taken when using a value > 1 because of the risk of
#'   removing real data points.
#' @param force_positive The default is \code{TRUE}. Sometimes if smoothing data
#'   with steep gradients it is possible for predicted values to be negative.
#'   \code{force_positive = TRUE} ensures that predictions remain positive. This
#'   is useful for several reasons. First, with lots of missing data more
#'   interpolation is needed and this can result in artifacts because the
#'   predictions are too far from the original data. Second, if it is known
#'   beforehand that the data are all positive, then this option carries that
#'   assumption through to the prediction. The only likely time where setting
#'   \code{force_positive = FALSE} would be if background concentrations were
#'   first subtracted resulting in data that is legitimately negative. For the
#'   vast majority of situations it is expected that the user will not need to
#'   alter the default option.
#' @param k This is the smoothing parameter used by the \code{gam} function in
#'   package \code{mgcv}. Typically, value of around 100 (the default) seems to
#'   be suitable and will resolve important features in the plot. The most
#'   appropriate choice of \code{k} is problem-dependent; but extensive testing
#'   of polar plots for many different problems suggests a value of \code{k} of
#'   about 100 is suitable. Setting \code{k} to higher values will not tend to
#'   affect the surface predictions by much but will add to the computation
#'   time. Lower values of \code{k} will increase smoothing. Sometimes with few
#'   data to plot \code{polarPlot} will fail. Under these circumstances it can
#'   be worth lowering the value of \code{k}.
#' @param normalise If \code{TRUE} concentrations are normalised by dividing by
#'   their mean value. This is done \emph{after} fitting the smooth surface.
#'   This option is particularly useful if one is interested in the patterns of
#'   concentrations for several pollutants on different scales e.g. NOx and CO.
#'   Often useful if more than one \code{pollutant} is chosen.
#' @param ws_spread The value of sigma used for Gaussian kernel weighting of
#'   wind speed when \code{statistic = "nwr"} or when correlation and regression
#'   statistics are used such as \emph{r}. Default is \code{0.5}.
#' @param wd_spread The value of sigma used for Gaussian kernel weighting of
#'   wind direction when \code{statistic = "nwr"} or when correlation and
#'   regression statistics are used such as \emph{r}. Default is \code{4}.
#' @param x_error The \code{x} error / uncertainty used when \code{statistic =
#'   "york_slope"}.
#' @param y_error The \code{y} error / uncertainty used when \code{statistic =
#'   "york_slope"}.
#' @param kernel Type of kernel used for the weighting procedure for when
#'   correlation or regression techniques are used. Only \code{"gaussian"} is
#'   supported but this may be enhanced in the future.
#' @param tau The quantile to be estimated when \code{statistic} is set to
#'   \code{"quantile.slope"}. Default is \code{0.5} which is equal to the median
#'   and will be ignored if \code{"quantile.slope"} is not used.
#' @param alpha The transparency of the plot. This is mainly useful to overlay
#'   it on a map.
#' @return A [ggplot2::ggplot2] figure
#'
#' @references
#'
#' Ashbaugh, L.L., Malm, W.C., Sadeh, W.Z., 1985. A residence time probability
#' analysis of sulfur concentrations at ground canyon national park. Atmospheric
#' Environment 19 (8), 1263-1270.
#'
#' Carslaw, D.C., Beevers, S.D, Ropkins, K and M.C. Bell (2006). Detecting and
#' quantifying aircraft and other on-airport contributions to ambient nitrogen
#' oxides in the vicinity of a large international airport.  Atmospheric
#' Environment. 40/28 pp 5424-5434.
#'
#' Carslaw, D.C., & Beevers, S.D. (2013). Characterising and understanding
#' emission sources using bivariate polar plots and k-means clustering.
#' Environmental Modelling & Software, 40, 325-329.
#' doi:10.1016/j.envsoft.2012.09.005
#'
#' Henry, R.C., Chang, Y.S., Spiegelman, C.H., 2002. Locating nearby sources of
#' air pollution by nonparametric regression of atmospheric concentrations on
#' wind direction. Atmospheric Environment 36 (13), 2237-2244.
#'
#' Henry, R., Norris, G.A., Vedantham, R., Turner, J.R., 2009. Source region
#' identification using Kernel smoothing. Environ. Sci. Technol. 43 (11),
#' 4090e4097. http:// dx.doi.org/10.1021/es8011723.
#'
#' Uria-Tellaetxe, I. and D.C. Carslaw (2014). Source identification using a
#' conditional bivariate Probability function. Environmental Modelling &
#' Software, Vol. 59, 1-9.
#'
#' Westmoreland, E.J., N. Carslaw, D.C. Carslaw, A. Gillah and E. Bates (2007).
#' Analysis of air quality within a street canyon using statistical and
#' dispersion modelling techniques. Atmospheric Environment. Vol.  41(39), pp.
#' 9195-9205.
#'
#' Yu, K.N., Cheung, Y.P., Cheung, T., Henry, R.C., 2004. Identifying the impact
#' of large urban airports on local air quality by nonparametric regression.
#' Atmospheric Environment 38 (27), 4501-4507.
#'
#' Grange, S. K., Carslaw, D. C., & Lewis, A. C. 2016. Source apportionment
#' advances with bivariate polar plots, correlation, and regression techniques.
#' Atmospheric Environment. 145, 128-134.
#' \url{https://www.sciencedirect.com/science/article/pii/S1352231016307166}
#'
#' @export
#' @family polar directional analysis functions
polar_plot <-
  function(data,
           pollutant,
           x = "ws",
           wd = "wd",
           facet = NULL,
           statistic = "mean",
           exclude_missing = TRUE,
           uncertainty = FALSE,
           percentile = NA,
           weights = c(0.25, 0.50, 0.75),
           min_bin = 1,
           force_positive = TRUE,
           k = 100,
           normalise = FALSE,
           ws_spread = 1.5,
           wd_spread = 5,
           x_error = NA,
           y_error = NA,
           kernel = "gaussian",
           tau = 0.5,
           alpha = 1) {
    # run original openair
    if (is.null(facet))
      facet <- "default"
    oa_data <-
      openair::polarPlot(
        mydata = data,
        pollutant = pollutant,
        x = x,
        wd = wd,
        type = facet,
        statistic = statistic,
        exclude_missing = exclude_missing,
        uncertainty = uncertainty,
        percentile = percentile,
        weights = weights,
        min.bin = min_bin,
        force.positive = force_positive,
        k = k,
        normalise = normalise,
        ws_spread = ws_spread,
        wd_spread = wd_spread,
        x_error = x_error,
        y_error = y_error,
        kernel = kernel,
        tau = tau,
        plot = FALSE
      )$data

    if (!"miss" %in% names(oa_data)) {
      oa_data[["miss"]] <- oa_data[["z"]]
    }

    plot_data <-
      oa_data %>%
      tidyr::drop_na("miss", "u", "v") %>%
      cart_to_polar() %>%
      dplyr::arrange("z")

    # fix multiple pollutants
    pollutant <- paste(pollutant, collapse = ", ")

    plt <-
      plot_polar(
        plot_data = plot_data,
        alpha = alpha,
        pollutant = pollutant,
        facet = facet,
        pointsize = 2
      )

    return(plt)
  }

#' K-means clustering of bivariate polar plots
#'
#' Function for identifying clusters in bivariate polar plots ([polarPlot()]);
#' identifying clusters in the original data for subsequent processing.
#'
#' Bivariate polar plots generated using the [polarPlot()] function provide a
#' very useful graphical technique for identifying and characterising different
#' air pollution sources. While bivariate polar plots provide a useful graphical
#' indication of potential sources, their location and wind-speed or other
#' variable dependence, they do have several limitations. Often, a `feature'
#' will be detected in a plot but the subsequent analysis of data meeting
#' particular wind speed/direction criteria will be based only on the judgement
#' of the investigator concerning the wind speed-direction intervals of
#' interest. Furthermore, the identification of a feature can depend on the
#' choice of the colour scale used, making the process somewhat arbitrary.
#'
#' \code{polarCluster} applies Partition Around Medoids (PAM) clustering
#' techniques to [polarPlot()] surfaces to help identify potentially interesting
#' features for further analysis. Details of PAM can be found in the
#' \code{cluster} package (a core R package that will be pre-installed on all R
#' systems). PAM clustering is similar to k-means but has several advantages
#' e.g. is more robust to outliers. The clustering is based on the equal
#' contribution assumed from the u and v wind components and the associated
#' concentration. The data are standardized before clustering takes place.
#'
#' The function works best by first trying different numbers of clusters and
#' plotting them. This is achieved by setting \code{n_clusters} to be of length
#' more than 1. For example, if \code{n_clusters = 2:10} then a plot will be
#' output showing the 9 cluster levels 2 to 10.
#'
#' The clustering can also be applied to differences in polar plot surfaces (see
#' [polarDiff()]). On this case a second data frame (`after`) should be
#' supplied.
#'
#' Note that clustering is computationally intensive and the function can take a
#' long time to run --- particularly when the number of clusters is increased.
#' For this reason it can be a good idea to run a few clusters first to get a
#' feel for it, e.g., \code{n_clusters = 2:5}.
#'
#' Once the number of clusters has been decided, the user can then run
#' [polar_cluster()] to return the original data frame together with a new
#' column `cluster`, which gives the cluster number as a character (see
#' example). Note that any rows where the value of `pollutant` is `NA` are
#' ignored so that the returned data frame may have fewer rows than the
#' original.
#'
#' Note that there are no automatic ways in ensuring the most appropriate number
#' of clusters as this is application dependent. However, there is often
#' a-priori information available on what different features in polar plots
#' correspond to. Nevertheless, the appropriateness of different clusters is
#' best determined by post-processing the data. The Carslaw and Beevers (2012)
#' paper discusses these issues in more detail.
#'
#' @inheritParams polar_plot
#' @param n_clusters Number of clusters to use. If `n_clusters` is more than
#'   length 1, then a faceted plot will be output showing the clusters
#'   identified for each one of `n_clusters`.
#' @param data_after Optional. Data representing the "after" case; see
#'   [polar_diff()] for more information.
#' @param return `"plot"` (the default) or `"data"`. `"plot"` will return
#'   plotted clusters for visual analysis so that an appropriate value for
#'   `n_clusters` can be selected. When such a value has been chosen, `"data"`
#'   will return the original data frame appended with a `cluster` column for
#'   use in, for example, [trend_prop()].
#' @inheritDotParams openair::polarPlot -mydata -pollutant -x -wd -type -limits
#'   -cols -mis.col -alpha -upper -angle.scale -units -key -key.header
#'   -key.footer -auto.text -plot
#' @family polar directional analysis functions
#' @family cluster analysis functions
#' @references
#'
#' Carslaw, D.C., Beevers, S.D, Ropkins, K and M.C. Bell (2006). Detecting and
#' quantifying aircraft and other on-airport contributions to ambient nitrogen
#' oxides in the vicinity of a large international airport.  Atmospheric
#' Environment. 40/28 pp 5424-5434.
#'
#' Carslaw, D.C., & Beevers, S.D. (2013). Characterising and understanding
#' emission sources using bivariate polar plots and k-means clustering.
#' Environmental Modelling & Software, 40, 325-329.
#' doi:10.1016/j.envsoft.2012.09.005
#' @export
polar_cluster <-
  function(data,
           pollutant,
           x = "ws",
           wd = "wd",
           n_clusters = 6,
           data_after = NA,
           return = "plot",
           ...) {
    if (return == "data" & length(n_clusters) > 1) {
      stop("To return data, please only provide one value for n_clusters.")
    }

    # run original openair
    if (return == "plot") {
      oa_data <-
        openair::polarCluster(
          mydata = data,
          x = x,
          pollutant = pollutant,
          wd = wd,
          n.clusters = n_clusters,
          after = data_after,
          plot = FALSE,
          plot.data = TRUE,
          ...
        )$data

      if (!"miss" %in% names(oa_data)) {
        oa_data[["miss"]] <- oa_data[["z"]]
      }

      plot_data <-
        oa_data %>%
        tidyr::drop_na("miss", "u", "v") %>%
        cart_to_polar() %>%
        dplyr::arrange("z")

      plt <- plot_polar(
        plot_data = plot_data,
        alpha = 1,
        pollutant = "cluster",
        facet = "default",
        color = "cluster",
        pointsize = 3
      ) +
        ggplot2::facet_wrap(ggplot2::vars(.data[["nclust"]]))

      return(plt)
    } else if (return == "data") {
      oa_data <-
        openair::polarCluster(
          mydata = data,
          x = x,
          pollutant = pollutant,
          wd = wd,
          n.clusters = n_clusters,
          after = data_after,
          plot = FALSE,
          plot.data = FALSE,
          ...
        )$data

      return(oa_data)
    }
  }

#' Polar plots considering changes in concentrations between two time periods
#'
#' This function provides a way of showing the differences in concentrations
#' between two time periods as a polar plot. There are several uses of this
#' function, but the most common will be to see how source(s) may have changed
#' between two periods.
#'
#' While the function is primarily intended to compare two time periods at the
#' same location, it can be used for any two data sets that contain the same
#' pollutant. For example, data from two sites that are close to one another, or
#' two co-located instruments.
#'
#' The analysis works by calculating the polar plot surface for the
#' \code{before} and \code{after} periods and then subtracting the \code{before}
#' surface from the \code{after} surface.
#'
#' @inheritParams polar_plot
#' @param data_before,data_after Data frames that represent the "before" and
#'   "after" cases. See [polar_plot()] for details of different input
#'   requirements.
#' @inheritDotParams openair::polarPlot -mydata -pollutant -x -wd -type -limits
#'   -cols -mis.col -alpha -upper -angle.scale -units -key -key.header
#'   -key.footer -auto.text -plot
#' @family polar directional analysis functions
#' @export
polar_diff <-
  function(data_before,
           data_after,
           pollutant,
           x = "ws",
           alpha = 1,
           ...) {
    # run original openair
    oa_data <-
      openair::polarDiff(
        before = data_before,
        after = data_after,
        pollutant = pollutant,
        x = x,
        plot = FALSE,
        ...
      )$data

    oa_data <-
      dplyr::mutate(oa_data, miss = .data$after - .data$before)

    plot_data <-
      oa_data %>%
      tidyr::drop_na("miss", "u", "v") %>%
      cart_to_polar() %>%
      dplyr::arrange("miss")

    # fix multiple pollutants
    pollutant <- paste(pollutant, collapse = ", ")

    plt <-
      plot_polar(
        plot_data = plot_data,
        alpha = alpha,
        pollutant = pollutant,
        facet = "default",
        color = "miss",
        pointsize = 2
      )

    return(plt)
  }

#' ggplot polar-type plots
#' @noRd
plot_polar <-
  function(plot_data,
           alpha,
           pollutant,
           facet,
           color = "z",
           pointsize = 1) {
    facet <- dplyr::group_vars(plot_data)
    for (i in facet) {
      if (is.factor(plot_data[i])){
        levels(plot_data[i]) <- quick_text(levels(plot_data[i]))
      } else {
        plot_data[i] <- quick_text(plot_data[[i]])
      }
    }

    plt <-
      ggplot2::ggplot(plot_data, ggplot2::aes(.data$t, .data$r)) +
      ggplot2::coord_polar() +
      scattermore::geom_scattermore(
        interpolate = TRUE,
        pointsize = pointsize,
        ggplot2::aes(color = .data[[color]]),
        na.rm = TRUE,
        alpha = alpha
      ) +
      ggplot2::scale_x_continuous(
        breaks = seq(0, 270, 90),
        limits = c(0, 360),
        labels = c("N", "E", "S", "W")
      ) +
      ggplot2::labs(
        x = NULL,
        y = NULL,
        color = openair::quickText(pollutant),
        fill = openair::quickText(pollutant)
      ) +
      ggplot2::expand_limits(y = 0)

    if (any(facet != "default")) {
      if (length(facet) == 1) {
        plt <-
          plt + ggplot2::facet_wrap(facets = ggplot2::vars(.data[[facet]]),
                                    labeller = ggplot2::label_parsed)
      } else {
        plt <-
          plt + ggplot2::facet_grid(
            cols = ggplot2::vars(.data[[facet[1]]]),
            rows = ggplot2::vars(.data[[facet[2]]]),
            labeller = ggplot2::label_parsed
          )
      }
    }
    return(plt)
  }

#' convert to polar coordinates
#' @noRd
cart_to_polar <- function(data) {
  dplyr::mutate(
    data,
    r = sqrt(.data$u ^ 2 + (.data$v * -1) ^ 2),
    t = dplyr::if_else(.data$u < 0,
                       atan((.data$v * -1) / .data$u) + pi,
                       atan((.data$v * -1) / .data$u)),
    t = (.data$t * (180 / pi)) + 90
  )
}
