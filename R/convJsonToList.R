#' @title The jsonToList function
#' @description Converts from JSON to list format in R
#' @param infile JSON source filename
#' @note code excerpted from http://www.r-bloggers.com/reading-files-in-json-format-a-comparison-between-r-and-python/
#' @keywords JSON conversion
#' @importFrom rjson fromJSON
#' @export
#' @examples
#' library(yelpRecommender)
#' fileloc <- system.file("extdata", "sample_business.json", package = "yelpRecommender")
#' busSampleList <- yelpRecommender::jsonToList(fileloc)
#' class(busSampleList)
#' names(busSampleList[[1]])

jsonToList <- function (infile) {
    if(!grepl(".json",infile,ignore.case = TRUE)) {
      stop("Files must have .json suffix")}
    json.file <- sprintf(infile, getwd())
    raw.json <- scan(json.file, what="raw()", sep="\n")
    lapply(raw.json, function(x) fromJSON(x))
}
