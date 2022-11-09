
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ggopenair

<!-- badges: start -->
<!-- badges: end -->

The goal of `ggopenair` is to bridge the gap between the data analysis
functions of `openair` and the flexibility of `ggplot2`. One of the key
strengths of `ggplot2` over `lattice` is that it allows plots to be
manipulated *after* they are created, which allows for a much more
flexible plotting experience. `ggplot2` is also being actively developed
and extended, which `ggopenair` can take advantage of.

## Installation

You can install the development version of ggopenair from
[GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("jack-davison/ggopenair")
```

## Example

An openair polar plot looks like this:

``` r
openair::polarPlot(openair::mydata)
```

<img src="man/figures/README-legacy-1.png" style="display: block; margin: auto;" />

To achieve the same result in `ggopenair` one would write:

``` r
library(ggopenair)

gg_polar_plot(openair::mydata, "nox") +
  theme_polar() +
  scale_opencolours()
```

<img src="man/figures/README-ggpolar-1.png" style="display: block; margin: auto;" />

This is more long winded, but the flexibility allows users to customise
their outputs very closely. For example:

-   **Custom Themes:** Use `ggplot2::theme()` or any theme package. For
    example, we can use the style from the Wall Street Journal:

``` r
gg_polar_plot(openair::mydata, "nox") +
  ggthemes::theme_wsj() +
  ggplot2::scale_color_gradientn(colours = rev(ggthemes::wsj_pal()(2))) +
  ggplot2::guides(color = ggplot2::guide_colorbar(barwidth = grid::unit(5, "cm")))
```

<img src="man/figures/README-themes-1.png" style="display: block; margin: auto;" />

-   **Scales:** Use any `ggplot2` scale function to change how the plot
    behaves. For example, use `scale_color_binned()` to bin the colour
    bar.

``` r
gg_polar_plot(openair::mydata, "nox") +
  theme_polar() +
  ggplot2::scale_color_binned(type = "viridis",
                              breaks = seq(0, 1000, 50))
```

<img src="man/figures/README-scales-1.png" style="display: block; margin: auto;" />

-   **Annotations:** Use `annotate()` to easily draw on your polar plots
    to draw attention to certain aspects.

``` r
gg_polar_plot(openair::mydata, "nox") +
  theme_polar() +
  scale_opencolours("inferno") +
  annotate_polar_wedge("S", "W") +
  ggplot2::annotate(
    geom = "text",
    x = (270+180)/2, y = 12, angle = -45,
    label = "Looks high here!", 
    color = "white"
  )
```

<img src="man/figures/README-annotations-1.png" style="display: block; margin: auto;" />

-   **Extensions:** Use any of the `ggplot2` extension packages out
    there, such as `patchwork`. For example, a polar plot could be
    inserted into a time variation plot:

``` r
library(patchwork)

polar <-
  gg_polar_plot(openair::mydata, "nox") + 
  theme_polar(guides = FALSE) + 
  ggplot2::theme(panel.border = ggplot2::element_rect(fill = NA, color = "black")) +
  scale_opencolours()

tv <- gg_timevariation(openair::mydata, "nox", return = "list")

(tv$day_hour + ggplot2::theme_bw() + ggplot2::theme(legend.position = "none")) /
  ((tv$month + ggplot2::theme_bw() + ggplot2::theme(legend.position = "none")) | 
     polar |
     (tv$day + ggplot2::theme_bw() + ggplot2::theme(legend.position = "none"))) +
  plot_layout(heights = c(.8, 1))
```

<img src="man/figures/README-patch-1.png" width="100%" style="display: block; margin: auto;" />
