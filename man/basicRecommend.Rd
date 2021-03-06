% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/basicRecommend.R
\name{basicRecommend}
\alias{basicRecommend}
\title{Recommendations based on ratings}
\usage{
basicRecommend(data, userID = NA, topNum = 10)
}
\arguments{
\item{data}{a joined data frame with columns named user_id, business_id, stars, 
including the user to whom the recommendation is given.}

\item{userID}{specify the user ID to whom the recommendation is given.}

\item{topNum}{specify the number of recommendations to be displayed. Default to be 10.}
}
\description{
Given a user ID which is contained in the given ratings data, the function
returns the top recommendations based on the user ratings.
}
\examples{
data(join100Sample)
id <- join100Sample$user_id[1]
basicRecommend(join100Sample, id)


}
\author{
Xueying Liu
}
\keyword{recommendation}

