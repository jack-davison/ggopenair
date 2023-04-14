#' Various colour scales from `openair`
#'
#' The original `openair` package contained the [openair::openColours()]
#' function which made many common colour palettes easily available. This is a
#' `ggplot2` scale wrapper around this function similar to
#' [ggplot2::scale_color_viridis_c()]. Unlike other `ggplot2` colour scale
#' functions, the `aesthetics` argument defaults to both "fill" and "colour",
#' meaning scales can be used with any `ggopenair` plotting function.
#'
#' @inheritParams openair::openColours
#' @inheritParams ggplot2::scale_color_viridis_c
#' @param ... Other arguments passed on to [ggplot2::discrete_scale()],
#'   [ggplot2::continuous_scale()], or [ggplot2::binned_scale()] to control
#'   name, limits, breaks, labels and so forth.
#' @seealso The documentation on [colour
#'   aesthetics][ggplot2::aes_colour_fill_alpha].
#' @rdname scale_opencolours
#' @export
scale_opencolours_d <-
  function(...,
           scheme = "default",
           aesthetics = c("colour", "fill")) {
    ggplot2::discrete_scale(aesthetics,
                            "opencolours_d",
                            openair_pal(colours = scheme),
                            ...)
  }

#' @export
#' @rdname scale_opencolours
scale_opencolours_c <-
  function(...,
           scheme = "default",
           values = NULL,
           space = "Lab",
           na.value = "grey50",
           guide = "colourbar",
           aesthetics = c("colour", "fill")) {
    ggplot2::continuous_scale(
      aesthetics,
      "opencolours_c",
      scales::gradient_n_pal(openair_pal(colours = scheme)(10),
                             values,
                             space),
      na.value = na.value,
      guide = guide,
      ...
    )
  }

#' @export
#' @rdname scale_opencolours
scale_opencolours_b <-
  function(...,
           scheme = "default",
           values = NULL,
           space = "Lab",
           na.value = "grey50",
           guide = "coloursteps",
           aesthetics = c("colour", "fill")) {
    pal <-
      binned_pal(openair_pal(colours = scheme))

    ggplot2::binned_scale(aesthetics,
                          "opencolours_b",
                          pal,
                          na.value = na.value,
                          guide = guide,
                          ...)
  }

#' Similar to viridis_pal from scales
#' @noRd
openair_pal <-
  function(colours = "default") {
    function(n) {
      openair::openColours(scheme = colours, n = n)
    }
  }

#' @author ggplot2 authors
#' @noRd
binned_pal <- function (palette) {
  function(x) {
    palette(length(x))
  }
}

#' @export
#' @rdname scale_opencolours
#' @usage NULL
scale_opencolors_d <- scale_opencolours_d

#' @export
#' @rdname scale_opencolours
#' @usage NULL
scale_opencolors_c <- scale_opencolours_c

#' @export
#' @rdname scale_opencolours
#' @usage NULL
scale_opencolors_b <- scale_opencolours_b
