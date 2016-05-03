if(getRversion() >= "2.15.1")  {
  utils::globalVariables(c("review_stars"))
}

#' @title The graphScoreDiff function
#' @description Creates histogram of differences between the maximum score for a cluster
#'   group and the minimum score for a different cluster group. Wide differences show 
#'   the clusters scoring a business differently
#' @author Ken Calhoon
#' @param fullclusterdata dataframe that includes business review information and cluster
#'   assignments
#' @return histogram plot of score differentials
#' @keywords plot
#' @importFrom dplyr group_by
#' @export
#' @examples
#' data("join100Sample")
#' data("user100Sample")
#' clusters <- 4
#' userFeatures <- appendUserData(fulldata = join100Sample, userdata = user100Sample)
#' dataCluster <- addCluster(join100Sample,appendKmeansCluster(userFeatures, k = clusters))
#' graphScoreDiff(dataCluster)
#' rm("join100Sample","user100Sample","clusters","userFeatures","dataCluster")

graphScoreDiff <- function (fullclusterdata) {
  colnames <- c("business_id","km_cluster","review_stars")
  if(!all(colnames %in% names(fullclusterdata))) {
    stop("dataframe must contain variables including business_id, km_cluster and review_stars")}
  clusters <- sum(unique(fullclusterdata$km_cluster))
  groupedData <- group_by(fullclusterdata,business_id)
  d <- summarize(groupedData, mean(review_stars[km_cluster==1]))
  for(x in seq(clusters-1)) {
    e <- summarize(groupedData, mean(review_stars[km_cluster == (x+1)]))
    d[,(x+2)] <- e[,2]
  }
  dCountISNA <- apply(d, 1, function (x) sum(is.na(x)))
  dNotNA <- d[dCountISNA <= (clusters-2),]
  dNotNA$max <- apply(dNotNA[,-1], 1, function(x) ifelse( !all(is.na(x)), max(x, na.rm=T), NA))
  dNotNA$min <- apply(dNotNA[,-1], 1, function(x) ifelse( !all(is.na(x)), min(x, na.rm=T), NA))
  dNotNA$diff <- dNotNA$max - dNotNA$min
  hist(dNotNA$diff,
       main = "Histogram of Difference in Cluster Score Min and Max",
       xlab = "Difference Between Min and Max Cluster Scores",
       col = "blue",
       border = "black")
}