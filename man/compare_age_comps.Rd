% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_om.R
\name{compare_age_comps}
\alias{compare_age_comps}
\title{Plot a comparison between age in catch for the OM and the old OM, which is read in as an rds object}
\usage{
compare_age_comps(
  om,
  country = NULL,
  season = 3,
  which_om = 0,
  fn = paste0("C:/github/pacific-hake/pacifichakemse/tests/testthat/om", which_om,
    "_old.rds")
)
}
\arguments{
\item{om}{Output from the \code{\link[=run_om]{run_om()}} function}

\item{country}{If NULL, a sum of both countries, if "ca" data for Canada and if "us", data for the US}

\item{season}{Numeric 1 through 4}

\item{which_om}{Which number OM to use. These correspond to selectivity change values}

\item{fn}{Location of the om_output.rds file from the old code"s base OM run}
}
\value{
a \code{\link[ggplot2:ggplot]{ggplot2::ggplot()}} object
}
\description{
Plot a comparison between age in catch for the OM and the old OM, which is read in as an rds object
}
