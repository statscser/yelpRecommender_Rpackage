#' @title Converting list to data frame
#' @description The function converts from R list object file to data frame for business, 
#' user, or review data.
#' @author Xueying Liu
#' @keywords conversion
#' @param data a JSON file of business, user, or review data.
#' @param type specify the data source type, either "business", "user", or "review".
#' @export
#' @examples
#' data(busSampleListData)
#' class(busSampleListData)
#' businessDF <- listToDataFrame(busSampleListData, type = "business")
#' class(businessDF)
#' str(businessDF)
#' 

listToDataFrame <- function(data, type = "") {
  if (!(type %in% c("business", "user", "review"))) {
    stop("Please specify the data source type: business/user/review")
  }

  if (type == "business") {
    df <- convertBusiness(data)
  } else if (type == "user") {
    df <- convertUser(data)
  } else {
    df <- convertReview(data)
  }
  
  return(df)
}



#' The convertBusiness function
#' Convert from JSON file to data frame for business data
#' @keywords conversion
#' @param data A JSON file of business information
#'

convertBusiness <- function(data) {
  id <- unlist(lapply(data, function(x) x$business_id))
  name <- unlist(lapply(data, function(x) x$name))
  city <- unlist(lapply(data, function(x) x$city))
  state <- unlist(lapply(data, function(x) x$state))
  # nb <- lapply(data, function(x) x$neighborhoods)
  # nb <- unlist(lapply(nb, function(x) ifelse(length(x) == 0, NA, x)))
  long <- unlist(lapply(data, function(x) x$longitude))
  lati <- unlist(lapply(data, function(x) x$latitude))
  cat <- lapply(data, function(x) x$categories)
  cat <- unlist(lapply(cat, function(x) paste0(x, sep = "|", collapse = "")))
  star <- unlist(lapply(data, function(x) x$stars))
  review <- unlist(lapply(data, function(x) x$review_count))
  review <- as.integer(review)
  
  price <- lapply(data, function(x) x$attributes$'Price Range')
  price <- unlist(lapply(price, function(x) ifelse(length(x) == 0, NA, x)))
  noise <- lapply(data, function(x) x$attributes$'Noise Level')
  noise <- unlist(lapply(noise, function(x) ifelse(length(x) == 0, NA, x)))
  alcohol <- lapply(data, function(x) x$attributes$Alcohol)
  alcohol <- unlist(lapply(alcohol, function(x) ifelse(length(x) == 0, NA, x)))
  kids <- lapply(data, function(x) x$attributes$'Good for Kids')
  kids <- unlist(lapply(kids, function(x) ifelse(length(x) == 0, NA, x)))
  groups <- lapply(data, function(x) x$attributes$'Good For Groups')
  groups <- unlist(lapply(groups, function(x) ifelse(length(x) == 0, NA, x)))
  romantic <- lapply(data, function(x) x$attributes$Ambience$romantic)
  romantic <- unlist(lapply(romantic, function(x) ifelse(length(x) == 0, NA, x)))
  intimate <- lapply(data, function(x) x$attributes$Ambience$intimate)
  intimate <- unlist(lapply(intimate, function(x) ifelse(length(x) == 0, NA, x)))
  classy <- lapply(data, function(x) x$attributes$Ambience$classy)
  classy <- unlist(lapply(classy, function(x) ifelse(length(x) == 0, NA, x)))
  hipster <- lapply(data, function(x) x$attributes$Ambience$hipster)
  hipster <- unlist(lapply(hipster, function(x) ifelse(length(x) == 0, NA, x)))
  divey <- lapply(data, function(x) x$attributes$Ambience$divey)
  divey <- unlist(lapply(divey, function(x) ifelse(length(x) == 0, NA, x)))
  touristy <- lapply(data, function(x) x$attributes$Ambience$touristy)
  touristy <- unlist(lapply(touristy, function(x) ifelse(length(x) == 0, NA, x)))
  trendy <- lapply(data, function(x) x$attributes$Ambience$trendy)
  trendy <- unlist(lapply(trendy, function(x) ifelse(length(x) == 0, NA, x)))
  upscale <- lapply(data, function(x) x$attributes$Ambience$upscale)
  upscale <- unlist(lapply(upscale, function(x) ifelse(length(x) == 0, NA, x)))
  casual <- lapply(data, function(x) x$attributes$Ambience$casual)
  casual <- unlist(lapply(casual, function(x) ifelse(length(x) == 0, NA, x)))
  
  df <- data.frame(business_id = id, bus_name = name, city = city, state = state,
                   longitude = long, latitude = lati,
                   categories = cat, bus_stars = star, bus_review_count = review,
                   price_range = price, noise_level = noise, alcohol = alcohol,
                   good_for_kids = kids, good_for_groups = groups,
                   romantic = romantic, intimate = intimate, classy = classy,
                   hipster = hipster, divey = divey, touristy = touristy, trendy = trendy,
                   upscale = upscale, casual = casual,
                   stringsAsFactors = FALSE)
  return(df)
}



#' The convertUser function
#' Convert from JSON file to data frame for user data
#' @keywords conversion
#' @param data A JSON file of user information
#'

convertUser <- function(data) {
  id <- unlist(lapply(data, function(x) x$user_id))
  name <- unlist(lapply(data, function(x) x$name))
  since <- unlist(lapply(data, function(x) x$yelping_since))
  star <- unlist(lapply(data, function(x) x$average_stars))
  review <- as.integer(unlist(lapply(data, function(x) x$review_count)))
  fan <- as.integer(unlist(lapply(data, function(x) x$fans)))
  friend <- lapply(data, function(x) x$friends)
  friend <- unlist(lapply(friend, function(x) paste0(x, sep = "|", collapse = "")))
  funny <- as.integer(unlist(lapply(data, function(x) x$votes$funny)))
  useful <- as.integer(unlist(lapply(data, function(x) x$votes$useful)))
  cool <- as.integer(unlist(lapply(data, function(x) x$votes$cool)))
  
  df <- data.frame(user_id = id, user_name = name, yelping_since = since, user_stars = star,
                   user_review_count = review, fans = fan, friends = friend,
                   user_votes_funny = funny, user_votes_useful = useful, user_votes_cool = cool,
                   stringsAsFactors = FALSE)
  return(df)
}



#' The convertReview function
#' Convert from JSON file to data frame for review data
#' @keywords conversion
#' @param data A JSON file of review information
#'

convertReview <- function(data) {
  r_id <- unlist(lapply(data, function(x) x$review_id))
  b_id <- unlist(lapply(data, function(x) x$business_id))
  u_id <- unlist(lapply(data, function(x) x$user_id))
  star <- unlist(lapply(data, function(x) x$stars))
  date <- unlist(lapply(data, function(x) x$date))
  text <- unlist(lapply(data, function(x) x$text))
  funny <- as.integer(unlist(lapply(data, function(x) x$votes$funny)))
  useful <- as.integer(unlist(lapply(data, function(x) x$votes$useful)))
  cool <- as.integer(unlist(lapply(data, function(x) x$votes$cool)))
  
  df <- data.frame(review_id = r_id, business_id = b_id, user_id = u_id, 
                   review_stars = star, date = date, text = text, 
                   review_votes_funny = funny, review_votes_useful = useful, review_votes_cool = cool,
                   stringsAsFactors = FALSE)
  return(df)
}


