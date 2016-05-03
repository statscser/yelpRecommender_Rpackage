if(getRversion() >= "2.15.1")  {
  utils::globalVariables(c("bus_stars", "pred.rating", "bus_name"))
}

#' @title Recommendations based on ratings
#' @description Given a user ID which is contained in the given ratings data, the function
#' returns the top recommendations based on the user ratings.
#' @author Xueying Liu
#' @param data a joined data frame with columns named user_id, business_id, stars, 
#' including the user to whom the recommendation is given.
#' @param  userID specify the user ID to whom the recommendation is given.
#' @param topNum specify the number of recommendations to be displayed. Default to be 10.
#' @keywords recommendation
#' @import recommenderlab Matrix reshape2
#' @importFrom dplyr %>% select arrange
#' @export
#' @examples
#' data(join100Sample)
#' id <- join100Sample$user_id[1]
#' basicRecommend(join100Sample, id)
#' 
#'

basicRecommend <- function(data, userID = NA, topNum = 10) {
  # check if the data is valid
  if (!all(c("user_id", "business_id", "review_stars") %in% names(data))) {
    stop("There must be columns of user_id, business_id and review_stars in the data frame!")
  }
  ratingList <- data[,c("user_id", "business_id", "review_stars")]
  
  # check if the userID is valid (should be included in the data)
  if (!userID %in% data$user_id) {
    stop("The specified user should be included in the data you provided!")
  }
  
  print("Calculating and generating the recommendations... be patient!")
  
  # predict ratings
  matrice <- as(ratingList, "realRatingMatrix")
  rec <- Recommender(matrice, method = "UBCF")
  pred <- predict(rec, matrice, type = "ratings")
  pred <- as(pred, "matrix")
  dimnames(pred) <- list(rownames(matrice), colnames(matrice))
  
  # order the recommended business for the specified user
  ratings <- sort(pred[userID,], decreasing = TRUE)
  busInfo <- unique(data %>% select(business_id, bus_name, bus_stars))
  rownames(busInfo) <- busInfo$business_id
  busInfo$pred.rating <- rep(0, nrow(busInfo))
  for (i in 1:nrow(busInfo)) {
    busInfo$pred.rating[i] <- ratings[busInfo$business_id[i]]
  }
  busInfo <- busInfo %>% arrange(desc(pred.rating))
  
  # generate top recommendations
  recom <- data.frame(business.name = busInfo$bus_name, stars = busInfo$bus_stars, 
                      predicted.rating = busInfo$pred.rating)
  if (nrow(recom) > topNum) {
    recom <- recom[1:topNum,]
  }
  
  return(recom)
}
