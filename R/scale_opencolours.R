#' Plotting with openair palettes in ggplot2
#'
#' This is a convenient wrapper around [ggplot2::scale_color_gradientn()] and
#' [openair::openColours()] which creates colour gradients for `ggopenair`
#' plots, with helpful defaults.
#'
#' @param colours The openair colour scheme to use. See [openair::openColours()]
#'   for more information.
#' @param na.value The aesthetic value to use for missing (NA) values. Defaults
#'   to "grey50".
#' @param aes Character string or vector of character strings listing the
#'   name(s) of the aesthetic(s) that this scale works with. The default,
#'   `c("colour", "fill")`, applies colours to both the `colour` *and* `fill`
#'   aesthetics.
#' @param ... Other arguments to pass to [ggplot2::scale_color_gradientn()].
#'
#' @return A ggproto object defining a continuous colour scale for use with
#'   [ggplot2::ggplot()].
#' @export
#'
#' @examples
#' \dontrun{
#' gg_polar(mydata, "nox") + scale_opencolours()
#' }
#'
scale_opencolours <-
  function(colours = "default",
           na.value = "grey50",
           aes = c("colour", "fill"),
           ...) {
    ggplot2::scale_color_gradientn(
      colours = openair::openColours(scheme = colours, n = 100),
      na.value = na.value,
      aesthetics = aes,
      ...
    )
  }
