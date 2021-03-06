% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AUDPCfun.R
\name{AUDPCfun}
\alias{AUDPCfun}
\title{An AUDPC function, calculating across many time-series simulations}
\usage{
AUDPCfun(reps, df)
}
\arguments{
\item{reps}{number of simulations}

\item{df}{dataframe with Rep #, "Number_of_Nodes" and "Step"}
}
\description{
This function allows you to calculate AUDPC across many reps of a dataframe generated from the function diseaseSpread, or DiseaseSpreadPWR
}
\examples{
AUDPCfun
}
\keyword{audpc}
\keyword{network}
\keyword{spread}
