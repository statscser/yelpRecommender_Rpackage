% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/convJsonToList.R
\name{jsonToList}
\alias{jsonToList}
\title{The jsonToList function}
\usage{
jsonToList(infile)
}
\arguments{
\item{infile}{JSON source filename}
}
\description{
Converts from JSON to list format in R
}
\note{
code excerpted from http://www.r-bloggers.com/reading-files-in-json-format-a-comparison-between-r-and-python/
}
\examples{
library(yelpRecommender)
fileloc <- system.file("extdata", "sample_business.json", package = "yelpRecommender")
busSampleList <- yelpRecommender::jsonToList(fileloc)
class(busSampleList)
names(busSampleList[[1]])
}
\keyword{JSON}
\keyword{conversion}

