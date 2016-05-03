#' @title The prepKmeansData function
#' @description Formats, scales (as appropriate) and selects variables for kmeans
#' @author Ken Calhoon
#' @param userdata dataframe of user data
#' @return dataframe ready for kmeans
#' @keywords data

prepKmeansData <- function(userdata) { #scales kmeans data and selects columns
  scale_vars <- c("user_stars","user_review_count","fans","user_votes_funny",
                  "user_votes_useful","user_votes_cool","review_count_geo","lon_mean",
                  "lat_mean","active_days","price_score","noise_score",
                  "alcohol_score")
  non_scale_vars <- c("female","kid_score","group_score","romantic_score",
                      "intimate_score","classy_score","hipster_score",
                      "divey_score","touristy_score","trendy_score",
                      "upscale_score","casual_score")
  dscale <- scale.default(userdata[,scale_vars])
  as.data.frame(cbind(dscale,userdata[,non_scale_vars]))
}


#' @title The appendKmeansCluster function
#' @description Appends to the input userdata file the cluster assignments
#' @author Ken Calhoon
#' @param userdata dataframe of user data
#' @param k (optional) number of clusters
#' @return dataframe with cluster assignments appended
#' @keywords data
#' @importFrom dplyr %>% 
#' @export
#' @examples
#' data("join100Sample")
#' data("user100Sample")
#' userFeatures <- appendUserData(fulldata = join100Sample, userdata = user100Sample)
#' userCluster <- appendKmeansCluster(userFeatures, k = 4)[,c("user_id","user_name","km_cluster")]
#' head(userCluster)
#' table(userCluster$km_cluster)
#' rm("join100Sample","user100Sample","userFeatures","userCluster")

appendKmeansCluster <- function(userdata, k = 4) {
  km_out <- userdata %>%
    prepKmeansData %>%
    kmeans(.,k)
  data.frame(userdata,km_cluster = km_out$cluster, stringsAsFactors = FALSE)
}