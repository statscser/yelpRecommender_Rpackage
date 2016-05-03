\name{yelpRecommender-package}
\alias{yelpRecommender-package}
\alias{yelpRocommender}
\docType{package}
\title{
  yelpRecommender: A Package to Deliver Customized Yelp Recommendations
}
\description{
  yelpRecommender includes functions to deliver recommendations more relevant to
  users based on their past scoring (recommendation engine) and user clustering.
}
\details{
The package has three main goals:
\itemize{
  \item Provide functions to wrangle the source JSON data into a usable format and subset: \code{\link{jsonToList}}, \code{\link{listToDataFrame}} and \code{\link{filterAndJoin}}
  \item Develop new, useful features from the underlying data: \code{\link{appendUserData}} 
  \item Provide recommendations using both traditional recommendation engine and
  cluster-based approaches: \code{\link{basicRecommend}}, \code{\link{recomByKeyWords}} and \code{\link{recommendByCluster}}
}
}
\author{
Xueying Liu \email{xyliu44@@stanford.edu}
Ken Calhoon \email{kcalhoon@@stanford.edu}

Maintainer: Xueying Liu \email{xyliu44@@stanford.edu}
Maintainer: Ken Calhoon \email{kcalhoon@@stanford.edu}
}
\references{
  The data to prototype the package comes from the Yelp Challenge Dataset \url{https://www.yelp.com/dataset_challenge}
  
}
% Optionally other standard keywords, one per line,
% from the file KEYWORDS in the R documentation.
\keyword{ package }


