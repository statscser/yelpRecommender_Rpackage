if(getRversion() >= "2.15.1")  {
  utils::globalVariables(c("state","city","business"))
}

#' @title Filtering business features and joining datasets
#' @description Given key words of business (state, city, type), the function return 
#' the joined data of review, business and user according to the filter conditions.
#' @author Xueying Liu
#' @param review_data a data frame of review information.
#' @param user_data a data frame of user information.
#' @param  business_data a data frame of business information.
#' @param state.name optional, specify the state of business to be filtered.
#' @param city.name optional, specify the city of business to be filtered.
#' @param type optional, specify the type (by a key word) of business to be filtered.
#' @keywords filter merge
#' @importFrom dplyr %>% filter left_join
#' @export
#' @examples
#' data(reviewSampleDataFrame)
#' data(busSampleDataFrame)
#' data(userSampleDataFrame)
#' joined <- filterAndJoin(reviewSampleDataFrame, userSampleDataFrame, busSampleDataFrame,
#' city.name = "Dravosburg")
#' str(joined)
#'

filterAndJoin <- function(review_data, user_data, business_data, 
                          state.name = NA, city.name = NA, type = NA) {
  
  # filter business corresponding to the key words specified
  business_filtered <- business_data
  if (!is.na(state.name)) {
    business_filtered <- business_filtered %>% filter(state == state.name)
  }
  if (!is.na(city.name)) {
    business_filtered <- business_filtered %>% filter(city == city.name)
  }
  if (!is.na(type)) {
    business_filtered <- business_filtered %>% 
      filter(grepl(type, business_filtered$categories, ignore.case = TRUE))
  }
  
  # join the review, user, business data after filtering
  review_filtered <- review_data %>% 
    filter(review_data$business_id %in% business_filtered$business_id)
  joined_data <- review_filtered %>% 
    left_join(user_data, by = "user_id") %>%
    left_join(business_filtered, by = "business_id")
  
  return(joined_data)
}

