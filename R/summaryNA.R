#' @title summaryNA
#'
#' @description
#' \code{summaryNA} takes a data frame and prints out summary statistics related to the amount of missing values in a data frame.
#'
#' @param df an object of class data.frame.
#' @param round rounding for summary statistics
#'
#' @export
#' @return an object of class matrix
#'
#' @examples
#' # library(seeNA)
#' # data("emptyCars")
#' # summaryNA(emptyCars)
summaryNA = function (df, round=0)
{
  if (!(is.data.frame(df))) {
    stop("You need to input a data frame")
  }
  dfname <- deparse(substitute(df))
  cat("\nThe data frame", dfname, "has",(nrow(df)*ncol(df)) , "total values")
  cat("\nMissing Values:",sum(is.na(df)))
  cat("\nPercentage of Missing Values:", paste0(round((sum(is.na(df))/(nrow(df)*ncol(df)))*100,round),  "%"))
  colNA <- function(df) {
    varnames <- colnames(df)
    tbl <- matrix(nrow = length(varnames), ncol = 3, byrow = TRUE)
    colnames(tbl) <- c("variable","n_missing", "pct_missing")
    rownames(tbl) <- varnames
    tbl[, 1] <- varnames
    for (i in 1:length(varnames)) {
      tbl[i, 2] <- sum(is.na(df[, i]))
    }
    n <- nrow(df)
    for (i in 1:length(varnames)) {
      tbl[i, 3] <- paste0(round(sum(is.na(df[, i])) * 100/n,round), "%")
    }
    cat("\n\n=================\n","Total Columns:", sum(colSums(is.na(df))!=0), sep = "")
    cat("\nMissing Columns:", sum(colSums(is.na(df))!=0), sep = "")
    cat("\nPercentage of Missing Columns:", paste0(round((sum(colSums(is.na(df))!=0)/ncol(df))*100,round),"%","\n\n"), sep = "")
    prmatrix(tbl, quote = FALSE, rowlab = rep("", nrow(tbl)),
             right = TRUE)
  }
  rowNA <- function(df) {
    total_rows = nrow(df)
    total_columns = ncol(df)
    n_missing_row = rowSums(is.na(df))
    list=replicate(12, 0)
    tbl <- matrix(nrow = 12, ncol = 3, byrow = TRUE)
    colnames(tbl) <- c("pct_missing", "n_rows", "pct_rows")
    tbl[, 1] <- c("= 0", "> 0%", "> 10%", "> 20%", "> 30%", "> 40%", "> 50%", "> 60%", "> 70%", "> 80%", "> 90%", "= 100%")
    for (missing in n_missing_row){
      if((missing/total_columns==0)){
        list[1]=list[1]+1
      }
      if((missing/total_columns>0 & missing/total_columns<=0.1)){
        list[2]=list[1]+1
      }
      if((missing/total_columns>0.1 & missing/total_columns<=0.2)){
        list[3]=list[1]+1
      }
      if((missing/total_columns>0.2 & missing/total_columns<=0.3)){
        list[4]=list[1]+1
      }
      if((missing/total_columns>0.3 & missing/total_columns<=0.4)){
        list[5]=list[1]+1
      }
      if((missing/total_columns>0.4 & missing/total_columns<=0.5)){
        list[6]=list[1]+1
      }
      if((missing/total_columns>0.5 & missing/total_columns<=0.6)){
        list[7]=list[1]+1
      }
      if((missing/total_columns>0.6 & missing/total_columns<=0.7)){
        list[8]=list[1]+1
      }
      if((missing/total_columns>0.7 & missing/total_columns<=0.8)){
        list[9]=list[1]+1
      }
      if((missing/total_columns>0.8 & missing/total_columns<=0.9)){
        list[10]=list[1]+1
      }
      if((missing/total_columns>0.9 & missing/total_columns<1)){
        list[11]=list[1]+1
      }
      if((missing/total_columns==1)){
        list[12]=list[1]+1
      }
    }
    for (i in 1:length(list)) {
      tbl[i, 2] <- list[i]
    }
    for (i in 1:length(list)) {
      tbl[i, 3] <- paste0(round(list[i]/total_rows * 100,round), "%")
    }
    cat("\n=================\n","Total Rows:", nrow(df), sep = "")
    cat("\nMissing Rows:", sum(rowSums(is.na(df))!=0), sep = "")
    cat("\nPercentage of Missing Rows:", paste0(round((sum(rowSums(is.na(df))!=0)/nrow(df))*100,round),"%"),"\n\n",sep = "")
    prmatrix(tbl, quote = FALSE, rowlab = rep("", nrow(tbl)),
             right = TRUE)
  }
  colNA(df)
  rowNA(df)
}
