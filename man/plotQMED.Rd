% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotQMED.R
\name{plotQMED}
\alias{plotQMED}
\title{plotQMED}
\usage{
plotQMED(x, ...)
}
\arguments{
\item{x}{A series of flow data loaded into the environment via HydroEnR}

\item{...}{Additional parameters as required}
}
\value{

}
\description{
Calculates and generates a plot of the QMED estimate relative
to the AMAX series.
}
\examples{
data <- getAMAX(rnrfa::get_ts(id = 2001, type = 'amax-flow'))
plotQMED(data)
}
