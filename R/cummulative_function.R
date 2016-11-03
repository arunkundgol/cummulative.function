#' Cummulative maximum
#' 
#' This function returns a vector whose elements are the maxima of the elements of the argument for date object
#' @param x vector of date object
#' @keywords cummax_date
#' 
#' @examples 
#' cummax_date()
#' @export

cummax_date <- function(x=structure(c(NA, NA, NA, NA, 17126, 18351, 16839, 17621, 18337, 
                                      NA), class = "Date")){
  x <- ifelse(is.na(x), -Inf,as.integer(x))
  y <- as.Date(cummax(x),'1970-01-01')
  return(y)
} 

#' Cummulative minimum
#' 
#' This function returns a vector whose elements are the minima of the elements of the arguments for date object
#' @param x vector of date object
#' @keywords cummin_date
#' 
#' @examples 
#' cummin_date()
#' @export

cummin_date <- function(x=structure(c(NA, NA, NA, NA, 17126, 18351, 16839, 17621, 18337, 
                                      NA), class = "Date")){
  x<- ifelse(is.na(x), +Inf, as.integer(x))
  y <- as.Date(cummin(x),'1970-01-01')
  return(y)
} 
