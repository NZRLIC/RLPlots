% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/MCMCOptions.R
\docType{data}
\name{MCMCOptions}
\alias{MCMCOptions}
\title{MCMC options}
\format{An object of class \code{list} of length 4.}
\usage{
MCMCOptions
}
\description{
List of options to be passed to MCMC plotting functions. This list is loaded with \pkg{RLPlots} and
is called by each plotting function. It contains a list of values that can be modified by the user 
and passed to the plotting function. Elements in the list include the following:
\describe{
  \item{\code{n.post =12}}{number of plots in each window for histograms, trace plots and
running average/quantile plots}
  \item{\code{begin.newplot = c(1,39)}}{the column number where you want new plot}
  \item{\code{Rewrite = 1}}{use 1 to re-calculate running mean for diagnostic otherwise 0}
  \item{\code{running.mean = 50}}{the n for calculated running means in trace and running
average/quantile plots}
}
}
\keyword{datasets}

