% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils-apply-hcr-om.R
\name{apply_hcr_om}
\alias{apply_hcr_om}
\title{Calculate the catch value to apply to the current year in the OM loop.
TACs can be applied to the catch value also.}
\usage{
apply_hcr_om(
  om,
  yr = NULL,
  yr_ind = NULL,
  tac = NULL,
  catch_floor = NULL,
  hcr_lower = 0.1,
  hcr_upper = 0.4,
  hcr_fspr = 0.4,
  ...
)
}
\arguments{
\item{om}{Operating model list}

\item{yr}{The year to calculate the reference points for}

\item{yr_ind}{Index for the year \code{yr} in the om output}

\item{tac}{TAC values to apply to the catch before returning the function.}

\item{catch_floor}{The lowest catch to be allowed}

\item{hcr_lower}{If \code{hcr_apply} is \code{TRUE}, this is the lower limit for
the rule}

\item{hcr_upper}{If \code{hcr_apply} is \code{TRUE}, this is the upper limit for
the rule}

\item{hcr_fspr}{If \code{hcr_apply} is \code{TRUE}, this is the value to set the
Fspr at for catch above \code{hcr_upper}}

\item{...}{Absorbs arguments meant for other functions}
}
\value{
Catch for the next year
}
\description{
Calculate the catch value to apply to the current year in the OM loop.
TACs can be applied to the catch value also.
}
