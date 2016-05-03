if(getRversion() >= "2.15.1")  {
  utils::globalVariables(c("business_id","name.y","km_cluster","cluster_n",
                           "desc","cluster_score", "review_stars", 
                           "cluster_grp", "lat_lon", "total_score"))
}

#' @title The addCluster function
#' @description Adds (left_joins) the user_id's cluster assignment to the full review dataset
#' @author Ken Calhoon
#' @param joinedData dataframe of all reviews, which includes business and
#'  other user information
#' @param userProfile data frame that contains a user_id and cluster assignment
#' @param joinVariable (optional) variable that the left_join will be based on
#' @return dataframe that has review, business, user and cluster assignment
#' @keywords data
#' @importFrom dplyr left_join
#' @export
#' @examples
#' data("join100Sample")
#' data("user100Sample")
#' clusters <- 4
#' userFeatures <- appendUserData(fulldata = join100Sample, userdata = user100Sample)
#' userWithCluster <- appendKmeansCluster(userFeatures, k = clusters)
#' busWithCluster <- addCluster(join100Sample,userWithCluster)
#' length(names(join100Sample))
#' length(names(busWithCluster))
#' rm("join100Sample","user100Sample","clusters","userFeatures","userWithCluster","busWithCluster")

addCluster <- function (joinedData, userProfile, joinVariable = "user_id") {
  left_join(joinedData,userProfile[,c("user_id","km_cluster")],by = "user_id")
}


#' @title The recommendByCluster function
#' @description Produces the top recommendations for each cluster (value) in a list 
#' @author Ken Calhoon
#' @param joinedData dataset of reviews, business and user information
#' @param userData basic user dataset before feature and cluster addition
#' @param numberReco (optional) number of recommendations returned
#' @param minClusterReview (optional) minimum number of reviews, by cluster,
#'  for a business to be included on the list
#' @param clusters (optional) number of user clusters
#' @return list (of datatables) of recommended businesses in descending order by cluster
#' @keywords data
#' @importFrom dplyr %>% group_by summarize filter arrange slice
#' @export
#' @examples
#' data("join100Sample")
#' data("user100Sample")
#' clusters <- 4
#' recos <- recommendByCluster(joinedData = join100Sample, userData = user100Sample, 
#'   numberReco = 20, minClusterReview = 2, clusters = 4)
#' sapply(seq(clusters), function (x) {print(head(recos[[x]], n = 3)[,c("cluster_grp","name")])})
#' rm("clusters","recos","join100Sample","user100Sample")

recommendByCluster <- function (joinedData, userData, numberReco = 20, minClusterReview = 2, clusters = 4) {
  userFeatures <- appendUserData(fulldata = joinedData, userdata = userData)
  userCluster <- appendKmeansCluster(userFeatures, k = clusters)
  data <- addCluster(joinedData, userCluster)
  restList <- list()
  clusters <- order(unique(data$km_cluster))
  i <- 0
  for(cluster in clusters) {
    i <- i+1
    result <- data %>%
      group_by(business_id) %>% 
      summarize(n = n(), 
        cluster_grp = unique(cluster),
        name = unique(bus_name),
        lat_lon = toString(format(c(unique(latitude),unique(longitude)),digits = 4, nsmall = 4)),
        cluster_score = mean(review_stars[km_cluster==cluster]),
        total_score = mean(review_stars),
        cluster_n = length(review_stars[km_cluster==cluster]),
        total_n = length(review_stars)) %>%
      na.omit %>%
      filter(cluster_n >= minClusterReview) %>%
      arrange(desc(cluster_score)) %>%
      slice(1:numberReco) %>%
      select(cluster_grp,name,lat_lon,cluster_score,total_score)
    restList[[i]] <- result
  }
  return(restList)
}


