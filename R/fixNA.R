#' @title fixNA
#'
#' @description
#' \code{fixNA} takes a data frame and transforms value_NA into recode_NA.
#'
#' @param data an object of class data.frame to transform.
#' @param value_NA single element or vector of elements to transform from. Default is 99 and NA.
#' @param recode_NA value to transform to. Default is NA.
#'
#' @export
#' @return an object of class data.frame
#' @examples
#' # data("randomCars")
#' # fixNA(randomCars)
fixNA = function(data=mtcars, value_NA = c(99,NA), recode_NA=NA){
  x=1
  for (i in 1:ncol(data)){
    y=1
    if (TRUE %in% (value_NA %in% data[[x]])){
      for(i in 1:nrow(data)){
        if(TRUE %in% (data[[x]][y] %in% value_NA)){
          data[[x]][y]=recode_NA
        }
        y=y+1
      }
    }
    x=x+1
  }
  return(data)
}
