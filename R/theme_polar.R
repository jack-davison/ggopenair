#' Default Polar Plot Theme
#'
#' This is a complete theme which attempts to recreate the styling of
#' [openair::polarPlot()]. As an alternative to using this theme, you could use
#' [ggplot2::theme()] and [ggplot2::guides()].
#'
#' @param guides Resize the legend? Defaults to `TRUE`. `FALSE` may be useful if
#'   you are controlling the legend yourself with [ggplot2::theme()] and/or
#'   [ggplot2::guides()], but still want to apply the axis formatting.
#' @param panel_ontop Place panel grids on top of the plot geometries? The
#'   default, `TRUE`, can make polar plots easier to interpret, but this
#'   behaviour may not always be desired.
#'
#' @return A ggplot theme object defining for use with [ggplot2::ggplot()].
#' @export
theme_polar <- function(guides = TRUE, panel_ontop = TRUE) {
  # make theme
  theme <-
    ggplot2::theme_minimal() %+replace%
    ggplot2::theme(
      panel.ontop = panel_ontop,
      axis.line.x.top = ggplot2::element_blank(),
      panel.grid.minor.x = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_line(color = "grey75"),
      panel.grid.major.y = ggplot2::element_line(linetype = 3, color = "grey75"),
      axis.text.x = ggplot2::element_text(face = "bold", size = 12)
    )

  # optionally control guides
  if (guides) {
    theme <- list(
      theme,
      ggplot2::guides(
        color = ggplot2::guide_colorbar(
          frame.colour = "white",
          ticks.colour = "white",
          barheight = grid::unit(10, "cm")
        )
      )
    )
  }

  theme
}
