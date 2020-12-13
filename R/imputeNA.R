#' @title imputeNA
#'
#' @description
#' \code{imputeNA} imputes the missing values in a dataframe based on the specified imputation function.
#'
#' @param data an object of class data.frame to impute missing values.
#' @param function.imp An optional character string naming the imputation function. By default, the imputation function is mean.
#' @param ... additional arguments passed to the function.
#'
#' @import mice missForest Amelia
#'
#' @export
#'
#' @return list or data.frame object
#' @examples
#' # library(seeNA)
#' # data("emptyCars")
#' # imputeNA(emptyCars)
imputeNA = function(data=emptyCars, function.imp = "mean", ...){
  if (!(is.data.frame(data))) {
    stop("You need to input a data frame")
  }
  else if (function.imp=="mean"){
    meanNA = function(x) replace(x, is.na(x), mean(x, na.rm = TRUE, ...))
    data = replace(data, TRUE, lapply(data, meanNA))
  }
  else if (function.imp=="mode"){
    modeNA = function(x) replace(x, is.na(x), mode(x, na.rm = TRUE, ...), ...)
    data = replace(data, TRUE, lapply(data, meanNA))
  }
  else if (function.imp=="median"){
    medianNA = function(x) replace(x, is.na(x), median(x, na.rm = TRUE, ...))
    data = replace(data, TRUE, lapply(data, meanNA))
  }
  else if (function.imp=="mice"){
    data.imp = mice(data,...)
    data <- mice::complete(data.imp,"all")
  }
  else if(function.imp=="missForest"){
    data.imp <- missForest(data,...)
    data <- data.imp$ximp
  }
  else if(function.imp=="Amelia"){
    data.imp = amelia(data, ...)
    data <- data.imp$imputations
  }
  return(data)
}
