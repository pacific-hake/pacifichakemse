% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/run_om.R
\name{run_om}
\alias{run_om}
\title{Run the Operating Model, which is an age based model
conditioned on true catch}
\usage{
run_om(
  om = NULL,
  yrs = om$yrs,
  random_seed = NULL,
  hcr_apply = FALSE,
  hcr_lower,
  hcr_upper,
  hcr_fspr,
  ...
)
}
\arguments{
\item{om}{list of parameters and life history values OM data. This list is
what is returned by \code{\link[=load_data_om]{load_data_om()}}}

\item{yrs}{A vector of the years to run}

\item{random_seed}{The random seed to use for this Operating Model}

\item{...}{Arguments passed to \code{\link[=run_year_loop_om]{run_year_loop_om()}}}
}
\value{
A list of model outputs
}
\description{
Run the Operating Model, which is an age based model
conditioned on true catch
}
