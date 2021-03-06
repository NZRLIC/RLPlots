% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/MPE_rules.R
\name{MPE_rules}
\alias{MPE_rules}
\title{Plot control rules}
\usage{
MPE_rules(control.pars, current.cpue = NA, cpue = seq(0, 2, 0.01),
  target.dir, file.suffix = "plot1")
}
\arguments{
\item{control.pars}{a matrix of control parameters with columns as parameters
and rows as different rules}

\item{cpue}{a vector of the catch per unit effort (CPUE) to be plotted as the x
axis}

\item{target.dir}{a character string of the target directory to save the plot to}

\item{file.suffix}{a suffix to append to the end of the saved plot name}

\item{obs.cpue}{the current CPUE}
}
\value{
a plot
}
\description{
Plot control rules
}
\author{
D'Arcy N. Webber, Charles Edwards
}

