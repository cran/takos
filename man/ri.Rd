% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Ri.R
\name{ri}
\alias{ri}
\title{Title running integral}
\usage{
ri(x, y, pks, TAP = FALSE, linear = FALSE, ...)
}
\arguments{
\item{x}{x axis for the intergration}

\item{y}{y axis for the intergration}

\item{pks}{selected peak}

\item{TAP}{if TRUE will apply a baseline using tangent area proportional (default=FALSE)}

\item{linear}{if TRUE  will apply a linear baseline (default=FALSE)}

\item{...}{parameters in TAPPA function}
}
\value{
\itemize{
\item ds data frame containing original x and y given as input
\item ri running integral
\item b.tap baseline calculate if the switch TAP is TRUE
\item y.tap = y - b.tap
}
}
\description{
calculate the running integral for the selected peak
}
\examples{
\donttest{
#' require(data.table)
require(MASS)
rates=c(0.5,1,2,5,10,20,50)
a<-lapply(rates, function(x) JMA(A=exp(35),Ea=120000,T0=0,T.end=300,q=x,npoints=5000,n=2))
a<-lapply(seq(1,length(a)), function(x) data.table(a[[x]]$time.s,a[[x]]$T.C,
a[[x]]$dadT, rates[[x]]))
lapply(seq(1,length(a)), function(x) setnames(a[[x]],
c("time.seconds","temperature.s","heat.flow","rates") ) )
a.dt <-lapply(seq(1,length(a)), function(x) data.table(data.frame(a.check[[x]])))
a<-rbindlist(a.dt)
a$rate<-a$id
a.peaks <- a[,.(res.list = list(findpeaks(heat.flow,sortstr=TRUE,npeaks=2))),by=id]
a.peaks$rate<-a.peaks$id
ref.peak=1
a.peaks <- data.table(data.table(a.peaks$rate),rbindlist((lapply(a.peaks$res.list,
function(x) data.table(t(x[ref.peak,]))))))
colnames(a.peaks)<- c("rate","peak.value","ind.max","left.lim","right.lim")
a.mat<- lapply(unique(a$rate),function(x)
ri(a[a$rate==x]$time.seconds,a[a$rate==x]$heat.flow,a.peaks[rate==x]))
}
}
