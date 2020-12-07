#' @title seeNA
#'
#' @description
#' \code{seeNA} takes a data frame and displays a visualization of the distribution of missing values within your data frame.
#'
#' @param data an object of class data.frame to visualize.
#' @param color color of points for visualization. Default is the color magenta.
#'
#' @import ggplot2 ggthemes
#'
#' @export
#'
#' @return NULL
#' @examples
#' # data(mtcars)
#' # x = makeNA(mtcars, n_missing = 50)
#' # seeNA(x)
seeNA = function(data, color = 'magenta'){
  data[is.na(data) == FALSE] = 1L
  data[is.na(data)] = 0L
  result_vec = c()
  for (i in 1:length(data)){
    total = length(data[[i]])
    sum = sum(data[[i]])
    result = (total - sum)/total
    result_vec[i] = result
  }
  result_df = as.data.frame(result_vec)
  row.names(result_df) = colnames(data)
  ggplot(result_df, aes(x = row.names(result_df), y = result_vec)) +
    geom_point(color = color, size = 3) +
    geom_text(aes(label = paste(round(result_vec * 100, 2),"%")), check_overlap = F) +
    ggtitle('NA Proportion per Variable Index') +
    scale_y_continuous(name = "Proportion of NA Values", limits = c(0,1)) +
    xlab('Variable Name(s)') +
    theme_economist() +
    scale_color_economist()
}

