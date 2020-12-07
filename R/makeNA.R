#' @title makeNA
#'
#' @description
#' \code{makeNA} takes a dataframe and converts n_missing random values into the default value of value_NA.
#'
#' @param data an object of class data.frame to apply missing values.
#' @param n_missing an object of class int specifying NA values in the returned data frame. Default is 10.
#' @param value_NA value to specified in the data frame. Default parameter is NA.
#'
#' @export
#' @return data.frame object
#' @examples
#' data(mtcars)
#' makeNA(mtcars)
makeNA = function(data,  n_missing = 10, value_NA = NA){
  holder = list()
  flag = 0
  i = 1
  while (i <= length(1:n_missing)){
    column_index = sample(1:length(data), 1)
    row_index = sample(1:nrow(data), 1)
    hold = append(column_index,row_index)
    for (j in holder){
      if (all(hold == j)){
        flag = 1
        break
      }
    }
    if (flag == 0) {
      data[[column_index]][row_index] = value_NA
      holder = append(holder, list(hold))
      i = i + 1
    }
    else {flag = 0}
  }
  return(data)
}
