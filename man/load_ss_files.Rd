% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ss_model_loading.R
\name{load_ss_files}
\alias{load_ss_files}
\title{Load all the SS files for output and input, and return the model object}
\usage{
load_ss_files(ss_model_output_dir = NULL, printstats = FALSE)
}
\arguments{
\item{ss_model_output_dir}{Directory the model resides in}

\item{printstats}{Print info on each model loaded via \code{\link[r4ss:SS_output]{r4ss::SS_output()}}}
}
\value{
A model object representing the output from the SS model
}
\description{
Load all the SS files for output and input, and return the model object
}
