% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/runningIntegral.R
\name{runningIntegral}
\alias{runningIntegral}
\title{Title running integral}
\usage{
runningIntegral(x, y, integrate.step = 1)
}
\arguments{
\item{x}{variable x use for integration process}

\item{y}{variable y use for integration process}

\item{integrate.step}{= the step used for calculating the integrale the default value is 1}
}
\description{
calculates the running integral for customer input
}
\examples{
npoints=1000
x=seq(1,npoints)
y=(dnorm(x, mean=npoints/2, sd=npoints/10))
runningIntegral(x,y)
}
