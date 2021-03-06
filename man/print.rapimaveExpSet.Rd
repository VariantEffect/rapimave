% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rapimave.R
\name{print.rapimaveExpSet}
\alias{print.rapimaveExpSet}
\title{Print R-API MaveDB ExperimentSet}
\usage{
\method{print}{rapimaveExpSet}(obj)
}
\arguments{
\item{obj}{the ExperimentSet object to print}
}
\description{
Prints a human-readable summary of a R-API MaveDB ExperimentSet object:
}
\examples{
\dontrun{
mave <- new.rapimave()
print(mave$getExperimentSet("urn:mavedb:00000001"))
}
}
\keyword{MaveDB}
