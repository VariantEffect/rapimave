% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rapimave.R
\name{new.genome}
\alias{new.genome}
\title{New R-API MaveDB genome}
\usage{
new.genome(data)
}
\value{
a new R-API MaveDB genome object.
}
\description{
Internal constructor that creates an immutable new genome object. This function is not exported
  and should not be called outside of the rapimave package itself. Genome objects are generated
  by the getGenome() method of a reference map object (which itself
 is generated using the \code{get.referenceMaps()} method on a target object). However, this section explains the
  functions that are available for these objects.
}
\details{
A ScoreSet object offers the following getter functions:
\itemize{
  \item{getShortName()} returns the name of this genome object
  \item{getSpecies()} returns the species to which this genome belongs.
  \item{getXRefEnsembl()} returns the a cross-reference object for Ensembl (if it exists).
  \item{getXrefRefseq()} returns the a cross-reference object for Refseq (if it exists).
}
}
