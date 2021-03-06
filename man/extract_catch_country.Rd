% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/extract_catch_country.R
\name{extract_catch_country}
\alias{extract_catch_country}
\title{Extract the catch by country using the assessment landings file}
\usage{
extract_catch_country(data_csv_dir = NULL)
}
\arguments{
\item{data_csv_dir}{The directory in which the assessment csv files containing catch by month are located}
}
\value{
A data frame with three columns, the year, the Canadian catch and the US catch
}
\description{
Extract the catch by country using the assessment landings file
}
