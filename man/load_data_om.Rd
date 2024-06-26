% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/load_dat_om.R
\name{load_data_om}
\alias{load_data_om}
\title{Prepare the data for the operating model}
\usage{
load_data_om(
  ss_model = NULL,
  yr_future = 0,
  n_sim_yrs = 0,
  n_season = 4,
  season_names = c("Season1", "Season2", "Season3", "Season4"),
  n_space = 2,
  space_names = c("Canada", "US"),
  n_survey = 2,
  ages = 0:20,
  age_plus_grp = 15,
  age_names = paste("age", 0:20),
  rdev_sd = 1.4,
  move_init = NULL,
  move_max_init = 0.35,
  move_fifty_init = 6,
  move_out = 0.85,
  move_south = 0.05,
  move_slope = 0.9,
  ages_no_move = 0:1,
  selectivity_change = 0,
  s_min = 1,
  s_max = 6,
  s_min_survey = 2,
  s_max_survey = 6,
  b_future = 0.5,
  sel_change_yr = 1991,
  f_space = c(0.2612, 0.7388),
  log_phi_survey = log(11.46),
  random_recruitment = TRUE,
  ...
)
}
\arguments{
\item{ss_model}{A model input/output list representing the SS model as
found in the RDS file created by  \code{\link[=create_rds_file]{create_rds_file()}}}

\item{yr_future}{Number of years to run the OM into the future in an OM
only run. If this is > 0, \code{n_sim_yrs} cannot be. This must be 0 when
running from an MSE context. Use \code{n_sim_yrs} for that instead.}

\item{n_sim_yrs}{Number of years to allocate OM objects for the future.
If this is > 0, \code{yr_future} cannot be. All future values will be \code{NA}
except for the movement matrix values.}

\item{n_season}{Number of seasons}

\item{season_names}{A vector of names for the seasons. Length must
equal \code{n_season}}

\item{n_space}{Number of spatial areas}

\item{space_names}{A vector of names for the spaces. Length must
equal \code{n_space}}

\item{n_survey}{Survey frequency}

\item{ages}{A vector of ages}

\item{age_plus_grp}{Plus group for ages. Required for
\code{\link[=setup_blank_om_objects]{setup_blank_om_objects()}}}

\item{age_names}{A vector of names for the ages. Length must equal
length of \code{ages}}

\item{rdev_sd}{Recruitment deviation Standard deviation}

\item{move_init}{Initial distribution of fish}

\item{move_max_init}{Maximum movement rate}

\item{move_fifty_init}{Age at 50 percent maximum movement rate}

\item{move_out}{Fraction of individuals that travel south in the last season}

\item{move_south}{Fraction of individuals that move south during the year}

\item{move_slope}{Slope of the movement function}

\item{ages_no_move}{Ages of fish which do not move in the movement model}

\item{selectivity_change}{Should selectivity change?}

\item{s_min}{Minimum age in fishery selectivity}

\item{s_max}{Maximum age in fishery selectivity}

\item{s_min_survey}{Minimum age in survey selectivity}

\item{s_max_survey}{Maximum age in survey selectivity}

\item{b_future}{Bias adjustment in the future}

\item{sel_change_yr}{A year in which a selectivity change took place}

\item{f_space}{The proportion of TAC given to each country. First value
is Canada, the second is the US}

\item{log_phi_survey}{Survey phi parameter value
If FALSE, they will be given random normal values based on rdev_sd}

\item{random_recruitment}{Logical. If \code{FALSE}, recruitment deviations
will be set to zero in the projection period. If \code{TRUE}, recruitment
deviations will be set to random values in the projection period}

\item{...}{Absorb arguments destined for other functions}
}
\value{
A list of Parameters, Input parameters, Survey, Catch, and others
}
\description{
Prepare the data for the operating model
}
\examples{
\dontrun{
df <- load_data_om(n_season = 2, n_space = 2)
}
}
