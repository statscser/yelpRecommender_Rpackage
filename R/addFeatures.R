if(getRversion() >= "2.15.1")  {
  appendUserDataVarNames <- c(".", "yelping_since", "user_id","name")
  featSumBusDataByUserVarNames <- c("price_range","noise_rating","alcohol_rating",
                                "good_for_kids","good_for_groups","romantic",
                                "intimate","classy","hipster","divey",
                                "touristy","trendy","upscale","casual","n",
                                "longitude","latitude")
  utils::globalVariables(c(appendUserDataVarNames,featSumBusDataByUserVarNames))
}

#' @title The convertDate function
#' @description Converts from character year & month ex. ("2008-11") to date format
#' @param data vector of dates in character format year-month format 
#' @return returns a date vector
#' @keywords date
#' @author Ken Calhoon

convertDate <- function (data) { 
  return(as.Date(paste(data,"-01",sep="")))
}


#' @title The featActiveDays function
#' @description Returns an integer vector with number of days since compDate
#' @param data vector of dates from which the number of days since compDate
#' @param compDate (optional) the date from which the input date is compared
#' @keywords feature
#' @author Ken Calhoon

featActiveDays <- function(data, compDate = "2016-01-01") {
  if(class(data) != "Date") {data <- as.Date(paste(data,"-01",sep=""))}
  return(as.integer(as.Date(compDate) - data))
}

#' @title The featPredFemale function
#' @description uses \code{\link[gender]{gender}} to predict gender based on first name
#' @param data character vector of first names
#' @param yearRange (optional) integer vector of start and stop date for name comparision
#' @return Returns boolean vector where 1 == female
#' @keywords feature
#' @author Ken Calhoon
#' @import gender
#' @import genderdata
#' @export
#' @examples
#' featPredFemale(c("Grace","Ken"))

featPredFemale <- function (data, yearRange = c(1950,1995)) {
  g <- gender(unique(data), yearRange, method = "ssa")[,c("name","gender")] #extracts name and gender vectors (only returns resolved names)
  names(g)[names(g)=="name"] <- "user_name"
  g$female <- as.integer(g$gender == "female")
  g$gender <- NULL
  return(g)
}  

#' @title The featSumBusDataByUser function
#' @description For each user_id, summarizes and scores the restaurant activity across many
#'  attributes
#' @param fulldata dataframe of all reviews, business and basic user data 
#'  be grouped by user_id
#' @keywords feature
#' @author Ken Calhoon
#' @importFrom dplyr %>% group_by summarize
featSumBusDataByUser <- function (fulldata) {
  convAlcoholNoiseToValue <- function(fulldata) { #converts character strings to values#
    fulldata$alcohol_rating <- ifelse(fulldata$alcohol == "none", 1L,
                                      ifelse(fulldata$alcohol == "bar_and_wine", 2L, 3L))
    fulldata$noise_rating <- ifelse(fulldata$noise_level == "quiet", 1L,
                                    ifelse(fulldata$noise_level == "average", 2L,
                                           ifelse(fulldata$noise_level == "loud", 3L, 4L)))
    return(fulldata)
  }
  
  fulldata %>% 
    convAlcoholNoiseToValue() %>%
    group_by(user_id) %>%
    summarize(
      price_score = mean(as.numeric(price_range)),
      noise_score = mean(as.numeric(noise_rating)),
      alcohol_score = mean(as.numeric(alcohol_rating)),
      kid_score = mean(as.numeric(good_for_kids)),
      group_score = mean(as.numeric(good_for_groups)),
      romantic_score = mean(as.numeric(romantic)),
      intimate_score = mean(as.numeric(intimate)),
      classy_score = mean(as.numeric(classy)),
      hipster_score = mean(as.numeric(hipster)),
      divey_score = mean(as.numeric(divey)),
      touristy_score = mean(as.numeric(touristy)),
      trendy_score = mean(as.numeric(trendy)),
      upscale_score = mean(as.numeric(upscale)),
      casual_score = mean(as.numeric(casual)),
      review_count_geo = n(),
      lon_mean = mean(longitude), 
      lat_mean = mean(latitude))
}

#' @title The appendUserData function
#' @description Wrapper function that appends user features. Uses \code{\link{convertDate}},
#'  \code{\link{featActiveDays}}, \code{\link{featPredFemale}}, 
#'  and \code{\link{featSumBusDataByUser}}.
#' @param fulldata dataframe with all reviews, business and basic user data
#' @param userdata dataframe with user profile data. New variables will be 
#'  appended to this dataframe
#' @return dataframe with new user features appended to the input "userdata"
#' @keywords data
#' @author Ken Calhoon
#' @importFrom dplyr %>% mutate left_join inner_join 
#' @export
#' @examples 
#' data("join100Sample")
#' data("user100Sample")
#' dim(user100Sample)
#' names(user100Sample)
#' u <- appendUserData(fulldata = join100Sample, userdata = user100Sample)
#' dim(u)
#' names(u)
#' rm("u", "join100Sample", "user100Sample")

appendUserData <- function(fulldata, userdata) {
  userdata <- userdata %>%
    dplyr::mutate(.,
                  yelping_since = convertDate(yelping_since),
                  active_days = featActiveDays(yelping_since))
  female <- featPredFemale(userdata$user_name)
  u <- (left_join(userdata,female,by = "user_name"))
  b <- featSumBusDataByUser(fulldata) 
  oldw <- getOption("warn")
  options(warn = -1)
  newu <- inner_join(u, b, by = "user_id") %>%
    lapply(., function (x) {x[is.na(x)] <- median(x,na.rm = TRUE);x}) %>% #replaces NAs with median
    as.data.frame %>%
    mutate(.,
           user_id = as.character(user_id),
           user_name = as.character(user_name))
  options(warn = oldw)
  return(newu)
}


