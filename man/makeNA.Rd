% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/makeNA.R
\name{makeNA}
\alias{makeNA}
\title{makeNA}
\usage{
makeNA(data, n_missing = 10, value_NA = NA)
}
\arguments{
\item{data}{an object of class data.frame to apply missing values.}

\item{n_missing}{an object of class int specifying NA values in the returned data frame. Default is 10.}

\item{value_NA}{value to specified in the data frame. Default parameter is NA.}
}
\value{
data.frame object
}
\description{
\code{makeNA} takes a dataframe and converts n_missing random values into the default value of value_NA.
}
\examples{
# data("mtcars")
# makeNA(mtcars)
}
