% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/LogiReg.R
\name{make_ones_and_zeroes}
\alias{make_ones_and_zeroes}
\title{Make Ones and Zeroes}
\usage{
make_ones_and_zeroes(y_pred, cutoff = 0.5)
}
\arguments{
\item{y_pred}{Column or list like object of elements on interval (0,1)}

\item{cutoff}{Default is 0.5. value of entry in ouput column is 1 if y_pred[i] >= cutoff. 0 Otherwise}
}
\value{
Returns a column of 1's and 0's same length as y_pred
}
\description{
Makes the column become 1's and 0's based on cutoff
}
\author{
Noah Heckenlively
}
