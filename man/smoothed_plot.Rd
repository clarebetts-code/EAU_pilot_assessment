% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pilot_assessment_functions.R
\name{smoothed_plot}
\alias{smoothed_plot}
\title{smoothed_plot}
\usage{
smoothed_plot(dat, long_term, short_term)
}
\arguments{
\item{dat}{a data frame, an element from dat_list}

\item{long_term}{Number of years of data to do the long term assessment from}

\item{short_term}{Number of years of data to do the short term assessment from}
}
\value{
a graph of the smoothed trend
}
\description{
called from within save_smothed_strend.
Visualises the smoothed trend for a variable
}
