% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_bars_country.R
\name{plot_bars_country}
\alias{plot_bars_country}
\title{Plot season indicators barplots by country and scenario}
\usage{
plot_bars_country(ps = NULL, facet_back_cols = c("red", "blue"), ...)
}
\arguments{
\item{ps}{A plot setup object as output by \code{\link[=setup_mse_plot_objects]{setup_mse_plot_objects()}}}

\item{facet_back_cols}{A vector of the facet text background colors}

\item{...}{Extra arguments to be passed to \code{\link[=color_facet_backgrounds]{color_facet_backgrounds()}}}
}
\value{
A \code{\link[ggplot2:ggplot]{ggplot2::ggplot()}} object
}
\description{
Plot season indicators barplots by country and scenario
}
