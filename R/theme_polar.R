#' Default Polar Plot Theme
#'
#' This is a complete theme which attempts to recreate the styling of
#' [openair::polarPlot()]. As an alternative to using this theme, you could use
#' [ggplot2::theme()] or any of the in-built `ggplot2` themes.
#'
#' @param panel_ontop Place panel grids on top of the plot geometries? The
#'   default, `TRUE`, can make polar plots easier to interpret, but this
#'   behaviour may not always be desired.
#'
#' @return A ggplot theme object defining for use with [ggplot2::ggplot()].
#' @export
theme_polar <- function(panel_ontop = TRUE) {
  # make theme
  theme <-
    ggplot2::theme_minimal() %+replace%
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = "white", color = NA),
      panel.ontop = panel_ontop,
      axis.line.x.top = ggplot2::element_blank(),
      panel.grid.minor.x = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_line(color = "grey75"),
      panel.grid.major.y = ggplot2::element_line(linetype = 3, color = "grey75"),
      axis.text.x = ggplot2::element_text(face = "bold", size = 12)
    )

  theme
}
