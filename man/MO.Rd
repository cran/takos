% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Mo.R
\name{MO}
\alias{MO}
\title{Title Mo model}
\usage{
MO(mat, degree = seq(0.2, 0.8, by = 0.2))
}
\arguments{
\item{mat}{matrix of the all the thermograms checked using the functiom mat.check}

\item{degree}{selected degrees of  cristallinity for performing the analysis}
}
\value{
models "mod", datable "xy" for plot, "Ea" list of value, datatable "DT" built with the values of mat according to the specified degrees
}
\description{
performs analysis of the thermograms using Mo method
}
\examples{
\donttest{
require(data.table)
require(MASS)
rates=c(0.5,1,2,5,10,20,50)
a<-lapply(rates, function(x) JMA(A=exp(35),Ea=120000,T0=0,T.end=300,q=x,npoints=5000,n=2))
a<-lapply(seq(1,length(a)), function(x) data.table(a[[x]]$time.s,a[[x]]$T.C,
a[[x]]$dadT, rates[[x]]))
lapply(seq(1,length(a)), function(x) setnames(a[[x]],
c("time.seconds","temperature.s","heat.flow","rates") ) )
ar<-testMat(a)
mo<-MO(ar)
}
}
\references{
Liu T, Mo Z, Wang S, Zhang H. Nonisothermal melt and cold crystallization kinetics of poly(aryl ether ether ketone ketone). Polym Eng Sci. 1997;37(3):568-575. doi:10.1002/pen.11700.
}
