if(getRversion() >= "2.15.1")  {
  utils::globalVariables(c("user_name", "user_stars", "user_review_count", "fans", "friends",
                           "user_votes_funny", "user_votes_useful", "user_votes_cool"))
}

#' @title Initializing user profile
#' @description The function takes an argument of the joined data converted from the 
#' "filterAndJoin" function, and initializes a user profile from the joined data.
#' @author Xueying Liu
#' @keywords user profile
#' @param joined.data a data frame of joined information converted from "filterAndJoin".
#' @importFrom dplyr %>% select
#' @export
#' @examples
#' data(join100Sample)
#' userProfile <- initUserProfile(join100Sample)
#' str(userProfile)
#' 
#' 

initUserProfile <- function(joined.data) {
  user <- unique(joined.data %>% 
                   select(user_id, user_name, yelping_since, user_stars, user_review_count,
                          fans, friends, user_votes_funny, user_votes_useful, user_votes_cool))
  # names(user) <- c("user_id", "user_name", "yelping_since", "user_stars", "user_review_count",
  #                  "fans", "friends", "user_votes_funny", "user_votes_useful", "user_votes_cool")
  return(user)
}
