if(getRversion() >= "2.15.1")  {
  utils::globalVariables(c("state", "city", "bus_stars", "categories", "bus_name"))
}

#' @title Recommendations based on given features
#' @description Given some key words about the business location or types, the function
#' returns the top recommendations for the user-specified condition.
#' @author Xueying Liu
#' @param data a data frame with business information.
#' @param state.name optional, specify the state of business to be recommended.
#' @param city.name optional, specify the city of business to be recommended.
#' @param types optional, a vector of characters to specify the types of business.
#' @param topNum specify the number of recommendations to be displayed. Default to be 10.
#' @keywords recommendation
#' @importFrom dplyr %>% filter select arrange
#' @export
#' @examples
#' data(busSampleDataFrame)
#' recomByKeyWords(busSampleDataFrame, types = "restaurant")
#'

recomByKeyWords <- function(data, state.name = NA, city.name = NA, types = NA, topNum = 10) {
  print("Searching... Please be patient for our recommendations for you!")
  business <- data
  
  # filter
  if (!is.na(state.name)) {
    business <- business %>% filter(state == state.name)
  }
  if (!is.na(city.name)) {
    business <- business %>% filter(city == city.name)
  }
  
  if (!is.na(types) && length(types) > 0) {
    for (word in types) {
      business <- business %>% 
        filter(grepl(word, business$categories, ignore.case = TRUE))
    }
  }
  
  # recommend
  if (nrow(business) == 0) {
    print("Sorry, there's no match of your search!")
  }
  
  business <- business %>% select(bus_name, bus_stars, categories) %>% arrange(desc(bus_stars))
  recom <- business
  names(recom) <- c("business.name", "stars", "categories")
  if (nrow(recom) > topNum) {
    recom <- recom[1:topNum,]
  }
  
  return(recom)
}


