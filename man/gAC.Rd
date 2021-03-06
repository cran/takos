% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gac.r
\name{gAC}
\alias{gAC}
\title{Title sbAC}
\usage{
gAC(time.start = 0, T0 = 0, T.end = 500, qqq = 50, A = 10^(6.3),
  Ea = 80000, m = 1, n = 2, K = 0, npoints = 10000,
  prec = 10^(-4.30095790876), rmod = "SB", ...)
}
\arguments{
\item{time.start}{Starting time for the simulations}

\item{T0}{Temperature start}

\item{T.end}{End temperature}

\item{qqq}{Heating rate}

\item{A}{Parameter in the equation}

\item{Ea}{Parameter in the equation}

\item{m}{Parameter in the equation}

\item{n}{Parameter in the equation}

\item{K}{Parameter in the equation}

\item{npoints}{Number of points}

\item{prec}{Starting value for the equation "prec"}

\item{rmod}{Kinetic model (default = Isoda)}

\item{...}{Parameters to pass to ode function for choosing solver method}
}
\value{
startgin temperature "T","fi",degree of crystallization "alfa",differential alfa in T "dadT",time in seconds "time.s",differential equation solution "sol"
}
\description{
Performs simulation according to several kinetic models
}
\examples{
 \donttest{

gAC(npoints=5000,prec=10^(-4.30095790876))

}
}
